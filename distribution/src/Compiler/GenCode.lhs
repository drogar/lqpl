\begin{code}
module Compiler.GenCode where
import Control.Monad.State
import Data.Map as Map
import Data.Char(toLower)
import Data.List as List

import Compiler.CompSupport(mkflabel, enumerateName) 
import Compiler.GenerationErrors
import Compiler.Instructions
import Compiler.IrTypes
import Compiler.Qtypes

import Data.Stack
import Data.Tuples



class GenCode a where
 genCode::a->CodeMonad ProgramCode
 genCodeList::[a]->CodeMonad ProgramCode
 genCodeList [] = return Map.empty
 genCodeList (a:as) = do x<- genCode a
		         xs<- genCodeList as
			 return $ combineProgs x xs

instance GenCode Iprog where
 genCode (Iprog mp) 
    = foldM (\i  -> liftM (Map.union i) . genCode ) Map.empty $ Prelude.map snd $ Map.toList mp



instance GenCode Iproc where
 genCode (Iproc nm lbl _ _ _ crets stmts) 
  = do pushProcName $ mkflabel nm lbl
       hcd <- genProcHeader
       unsetCurrentActive
       escop <- enscope
       cd <- genCode $ Iblock stmts
       creturn <- getClassicalRets $ fst $ unzip crets
       ret <- returnop (length crets)
       descp <- descope
       tcode <- genProcTrailer
       popProcName
       return $ combineProgs hcd $ 
              combineProgs escop $
              combineProgs cd $
              if nm == "main" then combineProgs descp tcode
                 else combineProgs creturn $
                      combineProgs descp $
                      combineProgs ret tcode 



instance GenCode Istmt where

 genCodeList [] = return Map.empty
 genCodeList (a:as) = do x<- genCode a
                         doPendingDiscards
		         xs<- genCodeList as
			 return $ combineProgs x xs

 genCode (IClassicalAssign nm off  expr) 
     = do ecode <- genCode expr
          tocstk <- classicalPut off
          return $ combineProgs ecode  tocstk

 genCode (Iassign nm (Left expr)) --Int
     = do ecode <- genCode expr
          tostk <- makeInt nm
          setCurrentActive nm
          return $ combineProgs ecode  tostk

 genCode (Iassign nm (Right expr@(IrQubit qv)))
     = do setCurrentActive nm
          allocQubit nm qv

 genCode (Iassign nm (Right expr@(IrExpCall _ frmlids _ _ _ )))
     = do ecode <- genCode expr
          let oldnm = last $ snd $ snd frmlids
          rn <- rename oldnm nm
          if oldnm == nm then return ecode
             else do reuseStackName oldnm
                     return $ combineProgs ecode  rn
 genCode (Iassign nm (Right expr@(IrVar oldnm _)))
     = do rn <- rename oldnm nm
          reuseStackName oldnm
          return rn

 genCode (Iassign nm (Right expr))
     = do ecode <- genCode expr
          oldnm <- getCurrentActive
          case oldnm of 
             Just oldname ->
                 do rn <- rename oldname nm
                    setCurrentActive nm
                    reuseStackName oldname
                    return $ combineProgs ecode  rn
             Nothing ->
                 fail unableToDetermineQstackTop

 genCode (IuseAssign nm (Right exp)) -- todo - see IUse
     = do icode <- genCode (Iassign nm (Right exp))
          unsetCurrentActive
          (startuse,enduse) <- delayedUse nm
          addDelayedCode enduse
          return $ combineProgs icode  startuse 

 genCode (IuseAssign nm (Left exp)) -- todo - see IUse
     = do --snm <- getStackName
          ecode <- genCode (Iassign nm (Left exp))
          --unsetCurrentActive
          (startuse,enduse) <- delayedUse nm
          addDelayedCode enduse
          return $ combineProgs ecode  startuse 
\end{code}
The new Iuse has expression to eval. The general structure
of the generated code will now be:
\begin{itemize}
\item{} expressionCode
\item{} use lbl
\item{} jump lbl2
\item{} lbl: Qdiscard
\item{} e1eval
\item{} CondJump lbla
\item{} stmts1
\item{} Jump lbl
\item{} lbla: ... Repeat last 5 items for each group of statements.
\item{} lbl: endQC
\item{} lbl2 : next....
\end{itemize}
\begin{code}

 genCode (Iuse [] _) = error iceNoidsWithUse

 genCode (Iuse [nm] stmts)
     = do usebdy <- genCode $ Iblock stmts
          lbl <- getLabel
          endlbl <- getLabel
          qendc <-  qend 
          escop <- enscope
          pu <- pullup nm
          useStrt <- useStartCode lbl endlbl nm
          bodystart <- labelizeM lbl $ discard nm
          endit <- labelizeM endlbl descope
          reuseStackName nm
          return $ combineProgs escop $
                 combineProgs pu $
                 combineProgs useStrt $
                 combineProgs bodystart $
                 combineProgs usebdy $
                 combineProgs qendc endit          


 genCode (Iuse (nm:nms) stmts)
     = do innerUse <- genCode (Iuse nms stmts)
          unsetCurrentActive
          lbl <- getLabel
          lbl2 <- getLabel
          escop <- enscope
          pu <- pullup nm
          useStrt <- useStartCode lbl lbl2 nm
          qendc <-  qend 
          bodystart <- labelizeM lbl $ discard nm
          endit <- labelizeM lbl2 descope
          reuseStackName nm
          return $  combineProgs escop $
                 combineProgs pu $
                 combineProgs useStrt $
                 combineProgs bodystart $
                 combineProgs innerUse $
                 combineProgs qendc endit          

 genCode (Iguard condstmts)
     = do endlbl <- getLabel
          usebdy <- guardBodyCode endlbl condstmts 
                    (genCode :: IrExpression -> CodeMonad ProgramCode)
                    (genCode :: Istmt -> CodeMonad ProgramCode)
          endit <- labelizeM endlbl nooperation
          return $ combineProgs usebdy endit          

 genCode (Imeas (IrVar nm t) s1 s2)
     =  do pu <- pullup nm
           mc <- measCode nm (genCode (Iblock s1)) (genCode (Iblock s2))
           return $ combineProgs pu mc


 genCode (Imeas e s1 s2)
     = do ecode <- genCode e
          nm <- getCurrentActive -- No pullup needed.
          case nm of
             Nothing   ->  error $ unsetTop e (Imeas e s1 s2)
             Just nm   ->  do  meascode <- measCode nm (genCode (Iblock s1)) (genCode (Iblock s2))
                               return $ combineProgs ecode  meascode

 genCode (Icase (IrVar nm t) clauses)
     = do pu <- pullup nm
          makeSplitCode pu nm clauses  (genCode :: Istmt -> CodeMonad ProgramCode)



 genCode (Icase e clauses)
     = do ec <- genCode e
          nm <- getCurrentActive --No pull up needed.
          case nm of 
            Nothing  ->  error $ unsetTop e (Icase e clauses)
            Just nm  ->  makeSplitCode ec nm clauses (genCode :: Istmt -> CodeMonad ProgramCode)


\end{code}
For |Icall| we need to first calculate any classical expressions
first, in the order presented. This is follwed by the 
quantum expression in \emph{reverse} order so that the first 
expression is on top. The names of each of these expressions must
then be saved as the outputs must be renamed to the output ids. 
This is follwed by the actual application of
the transform. Finally, the outputs are renamed.

\begin{code}


 genCode (Icall (Just ut) _ _ inCexps [IrVar oldnm _] [(newnm,_)] _) --ut no out classical ids
    = case ut of
         (Ident _) -> return Map.empty
         _ -> do eCcode <- mapM genCode inCexps -- $ reverse inCexps
                 aptran <- applyTransform (length inCexps) ut [oldnm]
                 renames <- rename oldnm newnm
                 removeFromPendingDiscards newnm
                 return $ combineProgs (combineAllProgs eCcode) $ 
                         combineProgs aptran renames

 genCode (Icall (Just ut) _ _ inCexps inQexps outQids _) --ut no out classical ids
    = case ut of
         (Ident _) -> return Map.empty
         _ -> do eCcode <- mapM genCode inCexps -- $ reverse inCexps
                 nmsAndEQcode <-  mapM quantifyAndName $ reverse inQexps
                 let (names,eQcode) = unzip nmsAndEQcode
                 aptran <- applyTransform (length inCexps) ut names
                 renames <- doRenames (reverse names) $ List.map fst outQids
                 mapM_ removeFromPendingDiscards $ List.map fst outQids
                 return $ combineProgs (combineAllProgs eCcode) $ 
                         combineProgs (combineAllProgs eQcode) $ 
                         combineProgs aptran renames


 genCode (Icall (Nothing) nm frmlids inCexps inQexps outQids outCids)
     = do eCcode <- mapM genCode inCexps -- TODO - check this $ reverse inCexps
          nmsAndEQcode <-  mapM quantifyAndName $ reverse inQexps
          let (names,eQcode) = unzip nmsAndEQcode
          renamesto <- doRenames (reverse names) $ fst $ snd frmlids
          callcd <- call (length inCexps) nm
          renamesfr <- doRenames (snd $ snd frmlids) $ List.map fst outQids
          unsetCurrentActive
          mapM_ removeFromPendingDiscards $ List.map fst outQids
          return $ combineProgs (combineAllProgs eCcode) $
                 combineProgs (combineAllProgs eQcode) $
                 combineProgs renamesto $
                 combineProgs callcd renamesfr


 genCode (Idiscard []) = return Map.empty
 genCode (Idiscard (nm:nms))
     = do dcode <- destroy  nm
          reuseStackName nm
          dcodes <- genCode (Idiscard nms)
          unsetCurrentActive
          return $ combineProgs dcode dcodes

 genCode (Ialloc nn  typ)
     = do acd <- alloc typ [nn]
          setCurrentActive nn
          return acd

 genCode (Iblock stmts)
     = do pushDelayedCode
          stms <- genCodeList stmts
          dc <- popDelayedCode
          return $ combineProgs stms dc

 genCode (IcontrolledBy stmts cids)
     = do cstart <- controlStartCode cids
          stms <- genCode $ Iblock stmts
          cend <- controlDoneCode
          return $ combineProgs cstart $
                 combineProgs stms cend

 genCode IzeroStack
     = zeroStackCode

 genCode Iskip
     = return Map.empty

 
quantAndName' :: IrExpression -> CodeMonad (NodeName, ProgramCode)
quantAndName' e
    = do exp <- genCode e
         nm <- getStackName
         tostk <- makeInt nm
         return (nm, 
                 combineProgs exp tostk) 
 
quantifyAndName :: IrExpression -> CodeMonad (NodeName, ProgramCode)
quantifyAndName  e@(Apply _ _ _)
    = quantAndName' e
quantifyAndName  e@(IrNot _ )
    = quantAndName' e
quantifyAndName  e@(IrMinus _ )
    = quantAndName' e
quantifyAndName  e@(IrBool _)
    = quantAndName' e
quantifyAndName  e@(IrNum _)
    = quantAndName' e
quantifyAndName  e@(IrCvar _ _ _ _)
    = quantAndName' e
quantifyAndName  e
    = codeAndName e
 
codeAndName :: IrExpression -> CodeMonad (NodeName, ProgramCode)
codeAndName e@(IrVar nm _)
    = do exp <- genCode e
         return (nm, exp)
codeAndName e@(IrExpCall nm frmlids inCexps inQexps outQids)
    = do exp <- genCode e
         let topnm = last $ snd $ snd frmlids
         return (topnm, exp)
codeAndName e
    = do exp <- genCode e
         lnm <- getCurrentActive
         case lnm of
             Nothing -> do lname <- getLastGenName
                           return (lname,exp)
             Just lname -> return (lname,exp)

instance GenCode IrExpression where
 genCode (Apply binop e1 e2)
     = do ex2 <- genCode e2 -- So second op is second on stack.
          ex1 <- genCode e1
          b <- genCode binop
          return $ combineProgs ex2 $ 
                 combineProgs ex1 b
 
 genCode (IrBool b)
     = cload $ Right b

 genCode (IrMinus exp)
     = do ex <- genCode exp
          mns <- minusSequence
          return $ combineProgs ex mns

 genCode (IrNot exp)
     = do ex <- genCode exp
          nt <- notInstruction
          return $ combineProgs ex nt
 genCode (IrVar nm t)
     = do  addToPendingDiscards nm
           return Map.empty

--error $ "Vars must be genned as part of other code" ++ nm
--do  addToPendingDiscards nm
 --          return plnm
                 

 genCode (IrCvar nn off l t)
     = classicalPull off

\end{code}
Can an expcall have classical parms?
\begin{code}

 genCode (IrExpCall nm frmlids inCexps inQexps outQids)
     = do eCcode <- mapM genCode inCexps
          nmsAndEQcode <-  mapM quantifyAndName $ reverse inQexps
          let (names,eQcode) = unzip nmsAndEQcode
          renamesto <- doRenames (reverse names) $ fst $ snd frmlids
          callcd <- call (length inCexps) nm
          renamesfr <- doRenames (snd $ snd frmlids) $ List.map fst outQids
          setCurrentActive $ last $ snd $ snd frmlids
          mapM_ removeFromPendingDiscards $ List.map fst outQids
          return $ combineProgs  (combineAllProgs eCcode) $
                 combineProgs  (combineAllProgs eQcode) $
                 combineProgs renamesto $
                 combineProgs callcd renamesfr

 genCode (IrQubit bv)
     = do nm <- getStackName
          setCurrentActive nm
          allocQubit nm bv

 genCode (IrNum num)
     = cload $ Left num

 genCode (IrCons cid exps)
     = do nmsAndEcode <-  mapM quantifyAndName exps
          let (names,ecode) = unzip nmsAndEcode
          name <- getStackName
          atyp <- allocType name cid
          bnds <- binds $ reverse names
          setCurrentActive name
          return $ combineProgs (combineAllProgs ecode) $
                 combineProgs atyp bnds

instance GenCode BinOp where
    genCode  = classicalOp

       
ioGenCode :: Iprog -> Int->IO Instructions
ioGenCode qprog loglevel
    = do (pc,s) <- runStateT (genCode qprog) 
		   (CodeState [] 0  [] "" 
                    emptyStack Map.empty emptyStack
                    Nothing loglevel )
         return $ progToIns pc

\end{code}
