open Format

type register_size = [ `b | `w | `l | `q | `d ]

let byte_size (s : register_size) = match s with
    `b -> 1
  | `w -> 2
  | `l -> 4
  | _ -> 8

type 'size register = string

let al : [ `b ] register = "%al"
let ah : [ `b ] register = "%ah"
let ax : [ `w ] register = "%ax"
let eax : [ `l ] register = "%eax"
let rax : [ `q ] register = "%rax"
let rax_  : ( [< `b | `w | `l | `q ] as 'a) -> 'a register = function
  | `b -> al
  | `w -> ax
  | `l -> eax
  | `q -> rax


let bl : [ `b ] register = "%bl"
let bh : [ `b ] register = "%bh"
let bx : [ `w ] register = "%bx"
let ebx : [ `l ] register = "%ebx"
let rbx : [ `q ] register = "%rbx"
let rbx_  : ( [< `b | `w | `l | `q ] as 'a) -> 'a register = function
  | `b -> bl
  | `w -> bx
  | `l -> ebx
  | `q -> rbx

let cl : [ `b ] register = "%cl"
let ch : [ `b ] register = "%ch"
let cx : [ `w ] register = "%cx"
let ecx : [ `l ] register = "%ecx"
let rcx : [ `q ] register = "%rcx"
let rcx_  : ( [< `b | `w | `l | `q ] as 'a) -> 'a register = function
  | `b -> cl
  | `w -> cx
  | `l -> ecx
  | `q -> rcx

let dl : [ `b ] register = "%dl"
let dh : [ `b ] register = "%dh"
let dx : [ `w ] register = "%dx"
let edx : [ `l ] register = "%edx"
let rdx : [ `q ] register = "%rdx"

let rdx_  : ( [< `b | `w | `l | `q ] as 'a) -> 'a register = function
  | `b -> dl
  | `w -> dx
  | `l -> edx
  | `q -> rdx



let sil : [ `b ] register = "%sil"
let si : [ `w ] register = "%si"
let esi : [ `l ] register = "%esi"
let rsi : [ `q ] register = "%rsi"
let rsi_  : ( [< `b | `w | `l | `q ] as 'a) -> 'a register = function
  | `b -> sil
  | `w -> si
  | `l -> esi
  | `q -> rsi

let dil : [ `b ] register = "%dil"
let di : [ `w ] register = "%di"
let edi : [ `l ] register = "%edi"
let rdi : [ `q ] register = "%rdi"
let rdi_  : ( [< `b | `w | `l | `q ] as 'a) -> 'a register = function
  | `b -> dil
  | `w -> di
  | `l -> edi
  | `q -> rdi

let bpl : [ `b ] register = "%bpl"
let bp : [ `w ] register = "%bp"
let ebp : [ `l ] register = "%ebp"
let rbp : [ `q ] register = "%rbp"
let rbp_  : ( [< `b | `w | `l | `q ] as 'a) -> 'a register = function
  | `b -> bpl
  | `w -> bp
  | `l -> ebp
  | `q -> rbp

let spl : [ `b ] register = "%spl"
let sp : [ `w ] register = "%sp"
let esp : [ `l ] register = "%esp"
let rsp : [ `q ] register = "%rsp"
let rsp_  : ( [< `b | `w | `l | `q ] as 'a) -> 'a register = function
  | `b -> spl
  | `w -> sp
  | `l -> esp
  | `q -> rsp


let r8b : [ `b ] register = "%r8b"
let r8w : [ `w ] register = "%r8w"
let r8d : [ `l ] register = "%r8d"
let r8 : [ `q ] register = "%r8"
let r8_  : ( [< `b | `w | `l | `q ] as 'a) -> 'a register = function
  | `b -> r8b
  | `w -> r8w
  | `l -> r8d
  | `q -> r8


let r9b : [ `b ] register = "%r9b"
let r9w : [ `w ] register = "%r9w"
let r9d : [ `l ] register = "%r9d"
let r9 : [ `q ] register = "%r9"
let r9_  : ( [< `b | `w | `l | `q ] as 'a) -> 'a register = function
  | `b -> r9b
  | `w -> r9w
  | `l -> r9d
  | `q -> r9



let r10b : [ `b ] register = "%r10b"
let r10w : [ `w ] register = "%r10w"
let r10d : [ `l ] register = "%r10d"
let r10 : [ `q ] register = "%r10"
let r10_  : ( [< `b | `w | `l | `q ] as 'a) -> 'a register = function
  | `b -> r10b
  | `w -> r10w
  | `l -> r10d
  | `q -> r10

let r11b : [ `b ] register = "%r11b"
let r11w : [ `w ] register = "%r11w"
let r11d : [ `l ] register = "%r11d"
let r11 : [ `q ] register = "%r11"
let r11_  : ( [< `b | `w | `l | `q ] as 'a) -> 'a register = function
  | `b -> r11b
  | `w -> r11w
  | `l -> r11d
  | `q -> r11


let r12b : [ `b ] register = "%r12b"
let r12w : [ `w ] register = "%r12w"
let r12d : [ `l ] register = "%r12d"
let r12 : [ `q ] register = "%r12"
let r12_  : ( [< `b | `w | `l | `q ] as 'a) -> 'a register = function
  | `b -> r12b
  | `w -> r12w
  | `l -> r12d
  | `q -> r12

let r13b : [ `b ] register = "%r13b"
let r13w : [ `w ] register = "%r13w"
let r13d : [ `l ] register = "%r13d"
let r13 : [ `q ] register = "%r13"
let r13_  : ( [< `b | `w | `l | `q ] as 'a) -> 'a register = function
  | `b -> r13b
  | `w -> r13w
  | `l -> r13d
  | `q -> r13

let r14b : [ `b ] register = "%r14b"
let r14w : [ `w ] register = "%r14w"
let r14d : [ `l ] register = "%r14d"
let r14 : [ `q ] register = "%r14"
let r14_  : ( [< `b | `w | `l | `q ] as 'a) -> 'a register = function
  | `b -> r14b
  | `w -> r14w
  | `l -> r14d
  | `q -> r14

let r15b : [ `b ] register = "%r15b"
let r15w : [ `w ] register = "%r15w"
let r15d : [ `l ] register = "%r15d"
let r15 : [ `q ] register = "%r15"
let r15_  : ( [< `b | `w | `l | `q ] as 'a) -> 'a register = function
  | `b -> r15b
  | `w -> r15w
  | `l -> r15d
  | `q -> r15

let xmm0 : [ `d ] register = "%xmm0"
let xmm1 : [ `d ] register = "%xmm1"
let xmm2 : [ `d ] register = "%xmm2"
let xmm3 : [ `d ] register = "%xmm3"
let xmm4 : [ `d ] register = "%xmm4"
let xmm5 : [ `d ] register = "%xmm5"
let xmm6 : [ `d ] register = "%xmm6"
let xmm7 : [ `d ] register = "%xmm7"
let xmm8 : [ `d ] register = "%xmm8"
let xmm9 : [ `d ] register = "%xmm9"
let xmm10 : [ `d ] register = "%xmm10"
let xmm11 : [ `d ] register = "%xmm11"
let xmm12 : [ `d ] register = "%xmm12"
let xmm13 : [ `d ] register = "%xmm13"
let xmm14 : [ `d ] register = "%xmm14"
let xmm15 : [ `d ] register = "%xmm15"

type label = string


type 'kind operand = string

type 'size reg = [ `reg of 'size ]
type imm = [ `imm ]
type address = [ `address ]

type 'size source = [ imm | address | 'size reg ]
type 'size dest = [  address | 'size reg ]

let (~$) (i : int)  : [`imm] operand =
  let max_i = 1 lsl 32 in
  let min_i = 1 lsl 31 in
  if i < max_i && i >= -min_i then
    sprintf "$%ld" (Int32.of_int i)
  else raise (Invalid_argument "~$: integer constant too large")

let (~:) (s : label)  : [`imm] operand = sprintf "$%s" s
let (~%) (r : ([< register_size] as 'a) register) : [ `reg of 'a ] operand = r

let addr ?(ofs=0) ?(scale=1)
    ?(index : [ `reg of [< register_size] ] operand option)
    (rb : [ `reg of [< register_size] ] operand)  : [`address] operand  =
     match index with
    None -> sprintf "%d(%s)" ofs rb
    | Some (ri) -> sprintf "%d(%s, %s, %d)" ofs rb (ri) scale


type 'a asm =
  | Nop
  | S of string
  | Cat of 'a asm * 'a asm

let nop = Nop
let inline s = S s
let (++) x y = Cat (x, y)

type text = [`text ] asm
type data = [`data ] asm

type program = {
  text : [ `text ] asm;
  data : [ `data ] asm;
}

let rec pr_asm fmt m =
  let open Format in
  match m with
  | Nop          -> ()
  | S s          -> fprintf fmt "%s" s
  | Cat (a1, a2) -> pr_asm fmt a1; pr_asm fmt a2

let print_program fmt p =
  let open Format in
  fprintf fmt ".text\n";
  pr_asm fmt p.text;
  fprintf fmt ".data\n";
  pr_asm fmt p.data;
  pp_print_flush fmt ()

let print_in_file ~file p =
  let open Format in
  let c = open_out file in
  let fmt = formatter_of_out_channel c in
  print_program fmt p;
  close_out c

let buf = Buffer.create 17
let fmt = formatter_of_buffer buf
let ins x =
  Buffer.add_char buf '\t';
  kfprintf (fun fmt ->
      fprintf fmt "\n";
      pp_print_flush fmt ();
      let s = Buffer.contents buf in
      Buffer.clear buf;
      S s
    ) fmt x


let label (s : label) : [> ] asm =
  S (s ^ ":\n")

let glabel (s : label) : [> ] asm =
  ins ".globl %s\n%s:" s s

let movb (src : [< `reg of [ `b ] | `imm | `address ] operand)
    (dst : [< `reg of [ `b ] | `address ] operand) : text =
  ins "movb %s, %s" src dst

let movw (src : [< `reg of [ `w ] | `imm | `address ] operand)
    (dst : [< `reg of [ `w ] | `address ] operand) : text =
  ins "movw %s, %s" src dst

let movl (src : [< `reg of [ `l ] | `imm | `address ] operand)
    (dst : [< `reg of [ `l ] | `address ] operand) : text =
  ins "movl %s, %s" src dst

let movq src dst =
  ins "movq %s, %s" src dst

let mov src dst =
  ins "mov %s, %s" src dst


let movsbw (src : [< `reg of [ `b ] | `imm | `address ] operand)
    (dst : [< `reg of [ `w ] | `address ] operand) : text =
  ins "movsbw %s, %s" src dst

let movsbl (src : [< `reg of [ `b ] | `imm | `address ] operand)
    (dst : [< `reg of [ `l ] | `address ] operand) : text =
  ins "movsbl %s, %s" src dst

let movsbq (src : [< `reg of [ `b ] | `imm | `address ] operand)
    (dst : [< `reg of [ `q ] | `address ] operand) : text =
  ins "movsbq %s, %s" src dst

let movswl (src : [< `reg of [ `w ] | `imm | `address ] operand)
    (dst : [< `reg of [ `l ] | `address ] operand) : text =
  ins "movswl %s, %s" src dst

let movswq (src : [< `reg of [ `w ] | `imm | `address ] operand)
    (dst : [< `reg of [ `q ] | `address ] operand) : text =
  ins "movswq %s, %s" src dst

let movslq (src : [< `reg of [ `l ] | `imm | `address ] operand)
    (dst : [< `reg of [ `q ] | `address ] operand) : text =
  ins "movslq %s, %s" src dst


(*| _ -> assert false*)


let movzbw (src : [< `reg of [ `b ] | `imm | `address ] operand)
    (dst : [< `reg of [ `w ] | `address ] operand) : text =
  ins "movzbw %s, %s" src dst

let movzbl (src : [< `reg of [ `b ] | `imm | `address ] operand)
    (dst : [< `reg of [ `l ] | `address ] operand) : text =
  ins "movzbl %s, %s" src dst

let movzbq (src : [< `reg of [ `b ] | `imm | `address ] operand)
    (dst : [< `reg of [ `q ] | `address ] operand) : text =
  ins "movzbq %s, %s" src dst

let movzwl (src : [< `reg of [ `w ] | `imm | `address ] operand)
    (dst : [< `reg of [ `l ] | `address ] operand) : text =
  ins "movzwl %s, %s" src dst

let movzwq (src : [< `reg of [ `w ] | `imm | `address ] operand)
    (dst : [< `reg of [ `q ] | `address ] operand) : text =
  ins "movzwq %s, %s" src dst

let movabsq (src : string) (dst : [< `reg of [`q] | `address ] operand) : text =
  ins "movabsq $%s, %s" src dst

let pushq (src :  [< `reg of [`q] | `imm | `address] operand ) : text =
  ins "pushq %s" src

let popq (dst : [< `reg of [`q ] | `address ] operand) : text =
  ins "popq %s" dst

let leab (src : [`address] operand)
    (dst : [`reg of [`b] ] operand) : text =
  ins "leab %s, %s" src dst

let leaw (src : [`address] operand)
    (dst : [`reg of [`w] ] operand ) : text =
  ins "leaw %s, %s" src dst

let leal (src : [`address] operand)
    (dst : [`reg of [`l] ] operand) : text =
  ins "leal %s, %s" src dst

let leaq (src : [`address] operand)
    (dst : [`reg of [`q] ] operand) : text =
  ins "leaq %s, %s" src dst

let lea src dst =
  ins "leaq %s, %s" src dst


let incb (dst : [< `reg of [`b] | `address ] operand) : text =
  ins "incb %s" dst

let incw (dst : [< `reg of [`w] | `address ] operand) : text =
  ins "incw %s" dst

let incl (dst : [< `reg of [`l] | `address ] operand) : text =
  ins "incl %s" dst

let incq (dst : [< `reg of [`q] | `address ] operand) : text =
  ins "incq %s" dst

let inc (dst) =
  ins "inc %s" dst


let decb (dst : [< `reg of [`b] | `address ] operand) : text =
  ins "decb %s" dst

let decw (dst : [< `reg of [`w] | `address ] operand) : text =
  ins "decw %s" dst

let decl (dst : [< `reg of [`l] | `address ] operand) : text =
  ins "decl %s" dst

let decq (dst : [< `reg of [`q] | `address ] operand) : text =
  ins "decq %s" dst

let dec (dst) =
  ins "dec %s" dst


let negb (dst : [< `reg of [`b] | `address ] operand) : text =
  ins "negb %s" dst

let negw (dst : [< `reg of [`w] | `address ] operand) : text =
  ins "negw %s" dst

let negl (dst : [< `reg of [`l] | `address ] operand) : text =
  ins "negl %s" dst

let neg (dst) =
  ins "neg %s" dst


let negq (dst : [< `reg of [`q] | `address ] operand) : text =
  ins "negq %s" dst

let notb (dst : [< `reg of [`b] | `address ] operand) : text =
  ins "notb %s" dst

let notw (dst : [< `reg of [`w] | `address ] operand) : text =
  ins "notw %s" dst

let notl (dst : [< `reg of [`l] | `address ] operand) : text =
  ins "notl %s" dst

let notq (dst : [< `reg of [`q] | `address ] operand) : text =
  ins "notq %s" dst

let not_ (dst) =
  ins "not %s" dst


let addb (src : [< `reg of [`b] | `imm | `address ] operand)
    (dst : [< `reg of [`b] | `address ] operand) : text =
  ins "addb %s, %s" src dst

let addw (src : [< `reg of [`w] | `imm | `address ] operand)
    (dst : [< `reg of [`w] | `address ] operand) : text =
  ins "addw %s, %s" src dst

let addl (src : [< `reg of [`l] | `imm | `address ] operand)
    (dst : [< `reg of [`l] | `address ] operand) : text =
  ins "addl %s, %s" src dst

let addq (src : [< `reg of [`q] | `imm | `address ] operand)
    (dst : [< `reg of [`q] | `address ] operand) : text =
  ins "addq %s, %s" src dst

let add (src) (dst) =
  ins "add %s, %s" src dst
    

let subb (src : [< `reg of [`b] | `imm | `address ] operand)
    (dst : [< `reg of [`b] | `address ] operand) : text =
  ins "subb %s, %s" src dst

let subw (src : [< `reg of [`w] | `imm | `address ] operand)
    (dst : [< `reg of [`w] | `address ] operand) : text =
  ins "subw %s, %s" src dst

let subl (src : [< `reg of [`l] | `imm | `address ] operand)
    (dst : [< `reg of [`l] | `address ] operand) : text =
  ins "subl %s, %s" src dst

let subq (src : [< `reg of [`q] | `imm | `address ] operand)
    (dst : [< `reg of [`q] | `address ] operand) : text =
  ins "subq %s, %s" src dst

let sub (src) (dst) =
  ins "sub %s, %s" src dst


let imulw (src : [< `reg of [`w] | `imm | `address ] operand)
    (dst : [`reg of [`w ]] operand) : text =
  ins "imulw %s, %s" src dst

let imull (src : [< `reg of [`l] | `imm | `address ] operand)
    (dst : [`reg of [`l]] operand) : text =
  ins "imull %s, %s" src dst

let imulq (src : [< `reg of [`q] | `imm | `address ] operand)
    (dst : [`reg of [`q]] operand) : text =
  ins "imulq %s, %s" src dst

let imul (src) (dst) =
  ins "imul %s, %s" src dst

let xorb (src : [< `reg of [`b] | `imm | `address ] operand)
    (dst : [< `reg of [`b] | `address ] operand) : text =
  ins "xorb %s, %s" src dst

let xorw (src : [< `reg of [`w] | `imm | `address ] operand)
    (dst : [< `reg of [`w] | `address ] operand) : text =
  ins "xorw %s, %s" src dst

let xorl (src : [< `reg of [`l] | `imm | `address ] operand)
    (dst : [< `reg of [`l] | `address ] operand) : text =
  ins "xorl %s, %s" src dst

let xorq (src : [< `reg of [`q] | `imm | `address ] operand)
    (dst : [< `reg of [`q] | `address ] operand) : text =
  ins "xorq %s, %s" src dst

let xor (src) (dst) =
  ins "xor %s, %s" src dst

let orb (src : [< `reg of [`b] | `imm | `address ] operand)
    (dst : [< `reg of [`b] | `address ] operand) : text =
  ins "orb %s, %s" src dst

let orw (src : [< `reg of [`w] | `imm | `address ] operand)
    (dst : [< `reg of [`w] | `address ] operand) : text =
  ins "orw %s, %s" src dst

let orl (src : [< `reg of [`l] | `imm | `address ] operand)
    (dst : [< `reg of [`l] | `address ] operand) : text =
  ins "orl %s, %s" src dst

let orq (src : [< `reg of [`q] | `imm | `address ] operand)
    (dst : [< `reg of [`q] | `address ] operand) : text =
  ins "orq %s, %s" src dst

let or_ (src) (dst) =
  ins "or %s, %s" src dst



let andb (src : [< `reg of [`b] | `imm | `address ] operand)
    (dst : [< `reg of [`b] | `address ] operand) : text =
  ins "andb %s, %s" src dst

let andw (src : [< `reg of [`w] | `imm | `address ] operand)
    (dst : [< `reg of [`w] | `address ] operand) : text =
  ins "andw %s, %s" src dst

let andl (src : [< `reg of [`l] | `imm | `address ] operand)
    (dst : [< `reg of [`l] | `address ] operand) : text =
  ins "andl %s, %s" src dst

let andq (src : [< `reg of [`q] | `imm | `address ] operand)
    (dst : [< `reg of [`q] | `address ] operand) : text =
  ins "andq %s, %s" src dst

let and_ (src) (dst) =
  ins "and %s, %s" src dst

let idivl (src : [< `reg of [`l] | `imm | `address ] operand) : text =
  ins "idivl %s" src

let divl (src : [< `reg of [`l] | `imm | `address ] operand) : text =
  ins "divl %s" src

let cltd : text =
  ins "cltd"

let idivq (src : [< `reg of [`q] | `imm | `address ] operand) : text =
  ins "idivq %s" src

let divq (src : [< `reg of [`q] | `imm | `address ] operand) : text =
  ins "divq %s" src

let cqto : text =
  ins "cqto"

let salb (src : [`imm] operand)
    (dst : [< `reg of [`b] | `address ] operand) : text =
  ins "salb %s, %s" src dst

let salw (src : [`imm] operand)
    (dst : [< `reg of [`w] | `address ] operand) : text =
  ins "salw %s, %s" src dst

let sall (src : [`imm] operand)
    (dst : [< `reg of [`l] | `address ] operand) : text =
  ins "sall %s, %s" src dst

let salq (src : [`imm] operand)
    (dst : [< `reg of [`q] | `address ] operand) : text =
  ins "salq %s, %s" src dst

let sal src dst =
  ins "sal %s, %s" src dst


let sarb (src : [`imm] operand)
    (dst : [< `reg of [`b] | `address ] operand) : text =
  ins "sarb %s, %s" src dst

let sarw (src : [`imm] operand)
    (dst : [< `reg of [`w] | `address ] operand) : text =
  ins "sarw %s, %s" src dst

let sarl (src : [`imm] operand)
    (dst : [< `reg of [`l] | `address ] operand) : text =
  ins "sarl %s, %s" src dst

let sarq (src : [`imm] operand)
    (dst : [< `reg of [`q] | `address ] operand) : text =
  ins "sarq %s, %s" src dst

let sar src dst =
  ins "sar %s, %s" src dst



let shrb (src : [`imm] operand)
    (dst : [< `reg of [`b] | `address ] operand) : text =
  ins "shrb %s, %s" src dst

let shrw (src : [`imm] operand)
    (dst : [< `reg of [`w] | `address ] operand) : text =
  ins "shrw %s, %s" src dst

let shrl (src : [`imm] operand)
    (dst : [< `reg of [`l] | `address ] operand) : text =
  ins "shrl %s, %s" src dst

let shrq (src : [`imm] operand)
    (dst : [< `reg of [`q] | `address ] operand) : text =
  ins "shrq %s, %s" src dst

let shr src dst =
  ins "shr %s, %s" src dst
    

let cmpb (src1 : [< `reg of [`b] | `imm | `address ] operand)
    (src2 : [< `reg of [`b] | `imm | `address ] operand) : text =
  ins "cmpb %s, %s" src1 src2

let cmpw (src1 : [< `reg of [`w] | `imm | `address ] operand)
    (src2 : [< `reg of [`w] | `imm | `address ] operand) : text =
  ins "cmpw %s, %s" src1 src2

let cmpl (src1 : [< `reg of [`l] | `imm | `address ] operand)
    (src2 : [< `reg of [`l] | `imm | `address ] operand) : text =
  ins "cmpl %s, %s" src1 src2

let cmpq (src1 : [< `reg of [`q] | `imm | `address ] operand)
    (src2 : [< `reg of [`q] | `imm | `address ] operand) : text =
  ins "cmpq %s, %s" src1 src2

let cmp src1 src2 =
  ins "cmp %s, %s" src1 src2


let testb (src1 : [< `reg of [`b] | `imm | `address ] operand)
    (src2 : [< `reg of [`b] | `imm | `address ] operand) : text =
  ins "testb %s, %s" src1 src2

let testw (src1 : [< `reg of [`w] | `imm | `address ] operand)
    (src2 : [< `reg of [`w] | `imm | `address ] operand) : text =
  ins "testw %s, %s" src1 src2

let testl (src1 : [< `reg of [`l] | `imm | `address ] operand)
    (src2 : [< `reg of [`l] | `imm | `address ] operand) : text =
  ins "testl %s, %s" src1 src2

let testq (src1 : [< `reg of [`q] | `imm | `address ] operand)
    (src2 : [< `reg of [`q] | `imm | `address ] operand) : text =
  ins "testq %s, %s" src1 src2

let test src1 src2 =
  ins "test %s, %s" src1 src2

let je (l : label) : text =
  ins "je %s" l

let jne (l : label) : text =
  ins "jne %s" l

let js (l : label) : text =
  ins "js %s" l

let jns (l : label) : text =
  ins "jns %s" l

let jg (l : label) : text =
  ins "jg %s" l

let jge (l : label) : text =
  ins "jge %s" l

let jl (l : label) : text =
  ins "jl %s" l

let jle (l : label) : text =
  ins "jle %s" l

let ja (l : label) : text =
  ins "ja %s" l

let jae (l : label) : text =
  ins "jae %s" l

let jb (l : label) : text =
  ins "jb %s" l

let jbe (l : label) : text =
  ins "jbe %s" l

let sete (d : [< `reg of [`b] | `address ] operand) : text =
  ins "sete %s" d

let setne (d : [< `reg of [`b] | `address ] operand) : text =
  ins "setne %s" d

let sets (d : [< `reg of [`b] | `address ] operand) : text =
  ins "sets %s" d

let setns (d : [< `reg of [`b] | `address ] operand) : text =
  ins "setns %s" d

let setg (d : [< `reg of [`b] | `address ] operand) : text =
  ins "setg %s" d

let setge (d : [< `reg of [`b] | `address ] operand) : text =
  ins "setge %s" d

let setl (d : [< `reg of [`b] | `address ] operand) : text =
  ins "setl %s" d

let setle (d : [< `reg of [`b] | `address ] operand) : text =
  ins "setle %s" d

let seta (d : [< `reg of [`b] | `address ] operand) : text =
  ins "seta %s" d

let setae (d : [< `reg of [`b] | `address ] operand) : text =
  ins "setae %s" d

let setb (d : [< `reg of [`b] | `address ] operand) : text =
  ins "setb %s" d

let setbe (d : [< `reg of [`b] | `address ] operand) : text =
  ins "setbe %s" d

(* cmov ? *)

let jmp (l : label) : text =
  ins "jmp %s" l

let jmp_star (a : [< `reg of [`q ] | `imm | `address ] operand) : text =
  ins "jmp *%s" a

let call (l : label) : text =
  ins "call %s" l

let call_star (a : [< `reg of [`q ] | `imm | `address ] operand) : text =
  ins "call *%s" a


let leave : text =
  ins "leave"

let ret : text =
  ins "ret"

type d_dest =  [`reg of [ `d ]]  operand

let movsd (d1 : [< `reg of [ `d ] | `address ] operand)
    (d2 : [< `reg of [ `d ] | `address ] operand) : text =
  ins "movsd %s, %s" d1 d2

let cvtsi2sd (src: [< `reg of [`l] | `address ] operand)
    (dst : [`reg of [ `d ]]  operand) : text =
  ins "cvtsi2sd %s, %s" src dst

let cvtsi2sdq (src: [< `reg of [`q] | `address ] operand)
    (dst : [`reg of [ `d ]]  operand) : text =
  ins "cvtsi2sdq %s, %s" src dst

let cvttsd2si (src: [< `reg of [`d] | `address ] operand)
    (dst : [ `reg of [`l]] operand ) : text =
  ins "cvttsd2si %s, %s" src dst

let cvttsd2siq (src: [< `reg of [`d] | `address ] operand)
    (dst : [`reg of [`q]] operand ) : text =
  ins "cvttsd2siq %s, %s" src dst

let addsd (src : [< `reg of [ `d ] | `address ] operand)
 (dst : [`reg of [ `d ]]  operand) : text =
  ins "addsd %s, %s" src dst

let subsd (src : [< `reg of [ `d ] | `address ] operand)
 (dst : [`reg of [ `d ]]  operand) : text =
  ins "subsd %s, %s" src dst

let mulsd (src : [< `reg of [ `d ] | `address ] operand)
 (dst : [`reg of [ `d ]]  operand) : text =
  ins "mulsd %s, %s" src dst

let divsd (src : [< `reg of [ `d ] | `address ] operand)
 (dst : [`reg of [ `d ]]  operand) : text =
  ins "divsd %s, %s" src dst

let minsd (src : [< `reg of [ `d ] | `address ] operand)
 (dst : [`reg of [ `d ]]  operand) : text =
  ins "minsd %s, %s" src dst

let maxsd (src : [< `reg of [ `d ] | `address ] operand)
 (dst : [`reg of [ `d ]]  operand) : text =
  ins "maxsd %s, %s" src dst

let sqrtsd (src : [< `reg of [ `d ] | `address ] operand)
 (dst : [`reg of [ `d ]]  operand) : text =
  ins "sqrtsd %s, %s" src dst

let ucomisd (s1 : [< `reg of [ `d ] | `address ] operand) (s2: [< `reg of [ `d ] | `address ] operand) : text =
  ins "ucomisd %s, %s" s1 s2


let comment s = S ("#" ^ s ^ "\n")

let align n = ins ".align %i" n

let pr_list fmt pr = function
  | []      -> ()
  | [i]     -> pr fmt i
  | i :: ll -> pr fmt i; List.iter (fun i -> fprintf fmt ", %a" pr i) ll

let pr_ilist fmt l =
  pr_list fmt (fun fmt i -> fprintf fmt "%i" i) l
;;

let pr_dlist fmt l =
  pr_list fmt (fun fmt d -> fprintf fmt "%f" d) l
;;

let pr_alist fmt l =
  pr_list fmt (fun fmt (a : string) -> fprintf fmt "%s" a) l

let dbyte l = ins ".byte %a" pr_ilist l
let dint  l = ins ".int %a" pr_ilist l
let dword l = ins ".word %a" pr_ilist l
let dquad l = ins ".quad %a" pr_ilist l
let ddouble l = ins ".double %a" pr_dlist l
let string s = ins ".string %S" s

let address l = ins ".quad %a" pr_alist l
let space n = ins ".space %d" n
let align n = ins ".align %d" n


let pushd src = subq ~$8 ~%rsp ++
                movsd src (addr ~%rsp)

let popd dst = movsd (addr ~%rsp) dst ++
               addq ~$8 ~%rsp
