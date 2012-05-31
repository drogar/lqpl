require 'exceptions/quantum_stack_invalid_create'
require 'exceptions/stack_descriptor_invalid_create'
require "model/stack_descriptor"
require "model/stack_zero"
require "model/stack_value"
require "model/stack_classical"
require "model/stack_qubit"
require "model/stack_data"
require "utility/drawing"

java_import com.drogar.qface.qstack.PaintMe
java_import java.awt.Dimension
java_import java.awt.BasicStroke
java_import java.awt.Color
java_import java.awt.Rectangle
java_import java.awt.Point
java_import java.awt.image.BufferedImage
java_import java.awt.geom.Line2D
java_import java.awt.AlphaComposite

class QuantumStack

  include PaintMe
  include Drawing
  attr_accessor :substacks
  attr_accessor :descriptor
  attr_accessor :name_memory_map

  def initialize(in_qstack, in_name_stack="")
    @bottom = in_qstack == "<bottom/>"
    if !bottom?
      md = SINGLE_QS_PATTERN.match in_qstack
      raise QuantumStackInvalidCreate, in_qstack if !md
      @stackaddress = md[1].to_i
      @on_diagonal = md[3] == "True"
      @substacks = QuantumStack::make_multiple_stacks(md[6], in_name_stack)
      @descriptor = StackDescriptor.make_instance md[7]
      case @descriptor
      when StackZero then  raise QuantumStackInvalidCreate, "Zero stack should not have substacks" if @substacks.size > 0
      when StackValue then raise QuantumStackInvalidCreate, "Value element should not have substacks" if @substacks.size > 0
      when StackQubit then raise QuantumStackInvalidCreate, "Qubit on stack should have substacks '#{in_qstack}" if @substacks.size == 0
      when StackClassical then raise QuantumStackInvalidCreate, "Classical element on stack should have substacks" if @substacks.size == 0
      when StackData then raise QuantumStackInvalidCreate, "Data element on stack should have substacks" if @substacks.size == 0
      end
      @memory_name_map = QuantumStack::decode_mmap in_name_stack
      @descriptor.name = @memory_name_map[@stackaddress]
    else
      @substacks = []
    end

  end

  def bottom?
    @bottom
  end

# PaintMe interface

  def paintme(g, p)
    d = get_preferred_size(g);
    if bottom?
      draw_centered_text(g,"...", d.width/2+10.0, d.height+40.0)
    else
      top_point = Point.new(d.width/2+10.0,d.height+40.0)
      paint_substacks(top_point,g)
      @descriptor.paintme_at_point(g,p,top_point)
    end
  end

  def paintmeAtPoint(g,p,center)
    if bottom?
      draw_text_centered_at_point(g,"...", center)
    else
      paint_substacks(center,g)
      @descriptor.paintme_at_point(g,p,center)

    end
  end

  alias :paintme_at_point :paintmeAtPoint

  # End PaintMe interface

  def paint_substacks(top_point,g)
    sign_control = (@substacks.length - 1) * 0.5
    basic_sizes = @substacks.collect { |sstack| [sstack.get_preferred_size(g).width, node_separation(:horizontal)].max}
    offset_sizes = QuantumStack::make_x_offsets basic_sizes
    sizes = QuantumStack::sum_x_offsets offset_sizes
    @substacks.each_with_index do |sstack,i|
      paint_point = Point.new(top_point.x + (i <=> sign_control)*sizes[i], top_point.y+node_separation(:vertical))
      ln = Line2D::Double.new(top_point, paint_point)
      g.set_color(Color.black)

      g.draw(ln)
      if i < sign_control
        where_to_draw = :left
      else
        where_to_draw = :right
      end
      draw_sized_text(g,8.0,"#{@descriptor.substack_labels[i]}",top_point, paint_point,where_to_draw)

      sstack.paintme_at_point(g,p,paint_point)

    end
  end

  def self.make_x_offsets(subs)
    len = subs.length
    return [0] if len == 1
    if len.even?
      bldr = subs.take(len/2)
      bldr << 0
      bldr += subs.drop(len/2)
    else
      bldr = subs.take(1 + len/2)
      bldr += subs.drop(len/2)
    end
    ret = []
    bldr.each_cons(2) {|pr| ret << (pr[0]+pr[1]) *0.5}
    ret[len/2] = 0 if len.odd?
    ret
  end

  def self.sum_x_offsets(offsets)
    return offsets if offsets.length < 4
    halflen = offsets.length / 2
    ret = []
    offsets.each_with_index do |item,i|
      if i < halflen
        r = offsets[i,halflen-i].inject(0) {|r,el| r+el}
      elsif i >= halflen
        r = offsets[halflen, i+1-halflen].inject(0) {|r,el| r+el}
      else
        r= item
      end
      ret << r
    end
    ret
  end

  def node_separation(direction)
    case direction
    when :vertical then 40.0
    when :horizontal then 55.0
    else 20.0
    end
  end


  def get_preferred_size(g)
    return g.get_font.get_string_bounds("...",g.get_font_render_context) if bottom?
    width = 0.0
    height = 0.0
    maxheight = 0.0
    @substacks.each do |sstack|
      dimsstack = sstack.get_preferred_size(g)
      width += [dimsstack.width, node_separation(:horizontal)].max
      maxheight = [maxheight, dimsstack.height + node_separation(:vertical)].max
    end
    height = maxheight
    dimnode = Dimension.new(0,0)
    dimnode.set_size(@descriptor.get_preferred_size(g)) if @descriptor
    d = Dimension.new(0,0)
    d.set_size([width,dimnode.width].max, [height,dimnode.height].max)
    d
  end

  def self.decode_mmap(in_mmap)
    return {} if in_mmap == ""
    ret = {}
    match_list_of_maps = MMAP_PATTERN.match in_mmap
    return {} if !match_list_of_maps
    lom = match_list_of_maps[1]
    list_elem = LIST_ELEMENT_PATTERN.match(lom)
    return {} if !list_elem
    list_elems_len = list_elem[0].length
    while list_elem
      ret.merge!(QuantumStack::kv_pairs_to_map(list_elem[1]))
      list_elem = LIST_ELEMENT_PATTERN.match(lom[list_elems_len, lom.length])
      return ret if !list_elem
      list_elems_len += list_elem[0].length
    end
  end

  def self.kv_pairs_to_map(kvps)
    return {} if !kvps or kvps == ""
    ret = {}
    kvp = KVPATTERN.match kvps
    return ret if !kvp
    ret[kvp[2].to_i] = kvp[1]
    matched_len = kvp[0].length
    while kvp
      kvp = KVPATTERN.match(kvps[matched_len,kvps.length])
      return ret if !kvp
      ret[kvp[2].to_i] = kvp[1]
      matched_len += kvp[0].length
    end
  end

  def self.make_multiple_stacks(many_stacks,in_name_stack="")
    return [] if many_stacks == ""
    next_stack = QuantumStack::get_next_qstack(many_stacks)
    raise InvalidInput, many_stacks if !next_stack
    rval=[QuantumStack.new(next_stack[0],in_name_stack)]
    while next_stack
      next_stack = QuantumStack::get_next_qstack(next_stack[1])
      return rval if !next_stack
      rval << QuantumStack.new(next_stack[0],in_name_stack)
    end
    rval
  end

  def self.get_next_qstack(multi_stacks)
    return nil if !multi_stacks or multi_stacks == ""
    if multi_stacks =~ /^<bottom\/>/
      return ["<bottom/>",multi_stacks[9,multi_stacks.length]]
    end
    if multi_stacks =~ /<Qstack/
      len = 8 # length of "<Qstack>"
      in_count = 1
      while in_count > 0
        len += 1
        in_count += 1 if multi_stacks[len,8] == "<Qstack>"
        in_count -= 1 if multi_stacks[len,9] == "</Qstack>"
      end
      len += 9
      return [multi_stacks[0,len], multi_stacks[len, multi_stacks.length - len + 1]]
    end
  end

  MMAP_PATTERN = Regexp.new /^<MMap>(.*)<\/MMap>$/
  LIST_ELEMENT_PATTERN = Regexp.new /^<map>(.*?)<\/map>/
  KVPATTERN = Regexp.new /^<kvpair><key><string>(.*?)<\/string><\/key><value><int>(\d*)<\/int><\/value><\/kvpair>/


  SINGLE_QS_PATTERN = Regexp.new /^<Qstack>
      <int>(-?(\d)*)<\/int>  #Stackaddress ([1])
      <bool>((True)|(False))<\/bool> #on diagonal ([3])
      <substacks>(.*)<\/substacks> # the substacks ([6])
      ((<Zero\/>)|(<Value>.*<\/Value>)|(<Qubits>.*<\/Qubits>)|(<ClassicalStack>.*<\/ClassicalStack>)|(<AlgebraicData>.*<\/AlgebraicData>))  # stack descriptor [7]
      <\/Qstack>/x



end