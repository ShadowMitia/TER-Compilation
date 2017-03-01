(** {0 Bibliothèque pour l'écriture de programmes X86-64 }

    Il s'agit là uniquement d'un fragment relativement petit
    de l'assembleur X86-64.

    Des types fantômes sont utilisés à outrance pour garantir autant
    que faire se peut que le code assembleur produit est bien formé.

    @author: 2015 Jean-Christophe Filliâtre (CNRS)
    @author: 2017 Kim Nguyễn (Université Paris-Sud)
*)

(** {1 Code } *)

type 'a asm
  (** type abstrait du code assembleur.
      Le paramètre ['a] est utilisé comme type fantôme. *)

type text = [ `text ] asm
  (** du code assembleur se trouvant dans la zone de texte *)

type data = [ `data ] asm
  (** du code assembleur se trouvant dans la zone de données *)

type label = string
  (** les étiquettes d'addresses sont des chaînes de caractères *)

val nop : [> ] asm
  (** l'instruction vide. Peut se trouver dans du text ou du data *)

val ( ++ ) : ([< `text|`data ] asm as 'a)-> 'a -> 'a
  (** concatène deux bouts de codes (soit text avec text, soit data avec
      data) *)

val inline: string -> [> ] asm
  (** [inline s] recopie la chaîne [s] telle quelle dans le fichier
      assembleur *)
val comment : string -> [> ] asm
(** insère un commentire *)
type program = {
  text : text;
  data : data;
}
  (** un programme est constitué d'une zone de texte et d'une zone de données *)

val print_program : Format.formatter -> program -> unit
  (** [print_program fmt p] imprime le code du programme [p] dans le
      formatter [fmt] *)

val print_in_file: file:string -> program -> unit

(** {1 Segment de données} *)
val dbyte : int list -> data
val dint : int list -> data
val dword : int list -> data
val dquad : int list -> data
val ddouble : float list -> data
val string : string -> data
val address : string list -> data
val space : int -> data
val align : int -> data

(** {1 Registres } *)


type register_size = [ `b | `w | `l | `q | `d ]
(** Variant polymorphe encodant les différentes tailles :
+ [`b] (byte) : octet
+ [`w] (word) : mot de 16 bits
+ [`l] (long word) : mot de 32 bits
+ [`q] (quad word) : mot de 64 bits
+ [`d] (double) : flottant double précision

*)

type 'size register
(** type abstrait représentant un registre de taille ['size] *)
val al  : [ `b ] register
val ah  : [ `b ] register
val ax  : [ `w ] register
val eax : [ `l ] register
val rax : [ `q ] register
val rax_: ([< `b | `w | `l | `q ] as 'a) -> 'a register


val bl  : [ `b ] register
val bh  : [ `b ] register
val bx  : [ `w ] register
val ebx : [ `l ] register
val rbx : [ `q ] register
val rbx_: ([< `b | `w | `l | `q ] as 'a) -> 'a register


val cl : [ `b ] register
val ch : [ `b ] register
val cx : [ `w ] register
val ecx : [ `l ] register
val rcx : [ `q ] register
val rcx_: ([< `b | `w | `l | `q ] as 'a) -> 'a register


val dl : [ `b ] register
val dh : [ `b ] register
val dx : [ `w ] register
val edx : [ `l ] register
val rdx : [ `q ] register
val rdx_: ([< `b | `w | `l | `q ] as 'a) -> 'a register


val sil : [ `b ] register
val si : [ `w ] register
val esi : [ `l ] register
val rsi : [ `q ] register
val rsi_: ([< `b | `w | `l | `q ] as 'a) -> 'a register



val dil : [ `b ] register
val di : [ `w ] register
val edi : [ `l ] register
val rdi : [ `q ] register
val rdi_: ([< `b | `w | `l | `q ] as 'a) -> 'a register

val bpl : [ `b ] register
val bp : [ `w ] register
val ebp : [ `l ] register
val rbp : [ `q ] register
val rbp_: ([< `b | `w | `l | `q ] as 'a) -> 'a register

val spl : [ `b ] register
val sp : [ `w ] register
val esp : [ `l ] register
val rsp : [ `q ] register
val rsp_: ([< `b | `w | `l | `q ] as 'a) -> 'a register

val r8b : [ `b ] register
val r8w : [ `w ] register
val r8d : [ `l ] register
val r8 : [ `q ] register
val r8_: ([< `b | `w | `l | `q ] as 'a) -> 'a register

val r9b : [ `b ] register
val r9w : [ `w ] register
val r9d : [ `l ] register
val r9 : [ `q ] register
val r9_: ([< `b | `w | `l | `q ] as 'a) -> 'a register

val r10b : [ `b ] register
val r10w : [ `w ] register
val r10d : [ `l ] register
val r10 : [ `q ] register
val r10_: ([< `b | `w | `l | `q ] as 'a) -> 'a register

val r11b : [ `b ] register
val r11w : [ `w ] register
val r11d : [ `l ] register
val r11 : [ `q ] register
val r11_: ([< `b | `w | `l | `q ] as 'a) -> 'a register

val r12b : [ `b ] register
val r12w : [ `w ] register
val r12d : [ `l ] register
val r12 : [ `q ] register
val r12_: ([< `b | `w | `l | `q ] as 'a) -> 'a register

val r13b : [ `b ] register
val r13w : [ `w ] register
val r13d : [ `l ] register
val r13 : [ `q ] register
val r13_: ([< `b | `w | `l | `q ] as 'a) -> 'a register

val r14b : [ `b ] register
val r14w : [ `w ] register
val r14d : [ `l ] register
val r14 : [ `q ] register
val r14_: ([< `b | `w | `l | `q ] as 'a) -> 'a register

val r15b : [ `b ] register
val r15w : [ `w ] register
val r15d : [ `l ] register
val r15 : [ `q ] register
val r15_: ([< `b | `w | `l | `q ] as 'a) -> 'a register
(** Les 16 registres généraux, avec leur sous-registres. Pour chaque registre 64bit [r],
    la fonction
    [r_ s] renvoie le registre de la bonne taille [s]. Par exemple :
    [r15_ `l] renvoie [r15d]
*)

val xmm0 : [ `d ] register
val xmm1 : [ `d ] register
val xmm2 : [ `d ] register
val xmm3 : [ `d ] register
val xmm4 : [ `d ] register
val xmm5 : [ `d ] register
val xmm6 : [ `d ] register
val xmm7 : [ `d ] register
val xmm8 : [ `d ] register
val xmm9 : [ `d ] register
val xmm10 : [ `d ] register
val xmm11 : [ `d ] register
val xmm12 : [ `d ] register
val xmm13 : [ `d ] register
val xmm14 : [ `d ] register
val xmm15 : [ `d ] register
(** registre flottants *)


(** {1 Opérandes } *)

type 'kind operand
(** type abstrait pour les opérandes *)

type 'size reg = [ `reg of 'size ]
type imm = [ `imm ]
type address = [ `address ]

type 'size source = [ imm | address | 'size reg ]
type 'size dest = [  address | 'size reg ]

(** type de variants polymorphes utilisés comme types fantômes *)


val ( ~$ ) : int -> [ `imm ] operand
(** [ ~$i ] opérande immédiate $i . *)

val ( ~: ) : label -> [ `imm ] operand
(** [ ~:l ] opérande immédiate crée à partir du label l. *)

val ( ~% ) : ([< register_size ] as 'a) register -> [ `reg of 'a ] operand
(** [ ~%r ] registre %r *)

val addr :
  ?ofs:int ->
  ?scale:int ->
  ?index:[ `reg of [< register_size ] ] operand ->
  [ `reg of [< register_size ] ] operand -> [ `address ] operand

(** [addr ~ofs:o ~scale:s ~index:~%ra ~%rb ]
    opérande addresse : o(%rb, %ra, s) *)

val label : label -> [>  ] asm
(** place une étiquette (dans une zone code ou de donnée) *)


val glabel : label -> [>  ] asm
(** place une étiquette précédée de .globl (dans une zone code ou de
    donnée) *)

(** {1 Instructions }

    Pour les instructions ci-dessous, on donne quand elles sont disponibles les versions
    specifiques ([movb], [movw], [movl], [movq] par exemple) et la version générique
    ([mov] par exemple). La version générique laisse l'assembleur déterminer la bonne
    instruction en fonction des opérandes (et affichera un message d'erreur si c'est
    impossible). Il faut en particulier que l'une des opérandes soit un registre
    pour que l'assembleur puisse déterminer la taille.


    Les instructions sont au format AT & T :  f src dst  == dst := (f src dst)

*)


(** {2 Transfert } *)

val movb: [< [ `b ] source ] operand -> [< [ `b ] dest ] operand -> text
val movw: [< [ `w ] source ] operand -> [< [ `w ] dest ] operand -> text
val movl: [< [ `l ] source ] operand -> [< [ `l ] dest ] operand -> text
val movq: [< [< `q | `d ] source ] operand -> [< [< `q | `d ] dest ] operand -> text
val mov: [< ([< `b | `w | `l | `q ] as 'a) source ] operand -> [< 'a dest ] operand -> text

(** attention, au plus un argument de type adresse.
*)

val movsbw: [< [ `b ] source ] operand -> [< [ `w ] dest ] operand -> text
val movsbl: [< [ `b ] source ] operand -> [< [ `l ] dest ] operand -> text
val movsbq: [< [ `b ] source ] operand -> [< [ `q ] dest ] operand -> text
val movswl: [< [ `w ] source ] operand -> [< [ `l ] dest ] operand -> text
val movswq: [< [ `w ] source ] operand -> [< [ `q ] dest ] operand -> text
val movslq: [< [ `l ] source ] operand -> [< [ `q ] dest ] operand -> text

(** 8->64 bit, avec extension de signe. *)


val movzbw: [< [ `b ] source ] operand -> [< [ `w ] dest ] operand -> text
val movzbl: [< [ `b ] source ] operand -> [< [ `l ] dest ] operand -> text
val movzbq: [< [ `b ] source ] operand -> [< [ `q ] dest ] operand -> text
val movzwl: [< [ `w ] source ] operand -> [< [ `l ] dest ] operand -> text
val movzwq: [< [ `w ] source ] operand -> [< [ `q ] dest ] operand -> text

(** 8->64 bit, avec extension par zéro *)

val movabsq: string -> [< [ `q ] dest ] operand -> text
(** chargement d'un immédiat 64 bits (à utiliser pour des constantes
    trop grande pour le type [int] d'OCaml *)


val pushq: [< [ `q ] source ] operand -> text
(** place l'opérande en sommet de pile (%rsp pointe sur la case occupée)
*)

val popq: [< [ `q ] dest ]  operand -> text
(** dépile et place la valeur à la destination donnée *)

(** {2 Arithmétique } *)

val leab: address operand -> [ `b ] reg operand -> text
val leaw: address operand -> [ `w ] reg operand -> text
val leal: address operand -> [ `l ] reg operand -> text
val leaq: address operand -> [ `q ] reg operand -> text
val lea: address operand -> [< ([ `b | `w | `l | `q ] as 'a) reg ] operand -> text
(** Calcul l'adresse correspondant à la première opérande et la place dans le registre destination *)

val incb: [< [ `b ] dest ] operand -> text
val incw: [< [ `w ] dest ] operand -> text
val incl: [< [ `l ] dest ] operand -> text
val incq: [< [ `q ] dest ] operand -> text
val inc: ([< `b | `w | `l | `q ] as 'a) reg operand -> text
(** incrémente l'opérande *)


val decb: [< [ `b ] dest ] operand -> text
val decw: [< [ `w ] dest ] operand -> text
val decl: [< [ `l ] dest ] operand -> text
val decq: [< [ `q ] dest ] operand -> text
val dec: ([< `b | `w | `l | `q ] as 'a) reg operand -> text
(** décrémente l'opérande *)

val negb: [< [ `b ] dest ] operand -> text
val negw: [< [ `w ] dest ] operand -> text
val negl: [< [ `l ] dest ] operand -> text
val negq: [< [ `q ] dest ] operand -> text
val neg: ([< `b | `w | `l | `q ] as 'a) reg operand -> text
(** l'opérande est remplacée par son opposé *)


val notb: [< [ `b ] dest ] operand -> text
val notw: [< [ `w ] dest ] operand -> text
val notl: [< [ `l ] dest ] operand -> text
val notq: [< [ `q ] dest ] operand -> text
val not_: ([< `b | `w | `l | `q ] as 'a) reg operand -> text
(** l'opérande est remplacée par sa négation bit à bit *)



val addb: [< [ `b ] source ] operand -> [< [ `b ] dest ] operand -> text
val addw: [< [ `w ] source ] operand -> [< [ `w ] dest ] operand -> text
val addl: [< [ `l ] source ] operand -> [< [ `l ] dest ] operand -> text
val addq: [< [ `q ] source ] operand -> [< [ `q ] dest ] operand -> text
val add: ([< `b | `w | `l | `q ] as 'a) reg operand -> 'a reg operand -> text
(** [dst <- src + dst] *)

val subb: [< [ `b ] source ] operand -> [< [ `b ] dest ] operand -> text
val subw: [< [ `w ] source ] operand -> [< [ `w ] dest ] operand -> text
val subl: [< [ `l ] source ] operand -> [< [ `l ] dest ] operand -> text
val subq: [< [ `q ] source ] operand -> [< [ `q ] dest ] operand -> text
val sub: ([< `b | `w | `l | `q ] as 'a) reg operand -> 'a reg operand -> text
(** [dst <- src - dst] *)

val imulw: [< [ `w ] source ] operand -> [ `w ] reg operand -> text
val imull: [< [ `l ] source ] operand -> [ `l ] reg operand -> text
val imulq: [< [ `q ] source ] operand -> [ `q ] reg operand -> text
val imul: ([< `w | `l | `q ] as 'a) reg operand -> 'a reg operand -> text
(** [dst <- src * dst] *)

val idivl: [< [ `l ] source ] operand -> text
val divl: [< [ `l ] source ] operand -> text
val cltd: text
(** division 32 bits *)

val idivq: [< [ `q ] source ] operand -> text
val divq: [< [ `q ] source ] operand -> text
val cqto: text
(** division 64 bits *)

(** {2 Opérations logiques } *)

val xorb: [< [ `b ] source ] operand -> [< [ `b ] dest ] operand -> text
val xorw: [< [ `w ] source ] operand -> [< [ `w ] dest ] operand -> text
val xorl: [< [ `l ] source ] operand -> [< [ `l ] dest ] operand -> text
val xorq: [< [ `q ] source ] operand -> [< [ `q ] dest ] operand -> text
val xor: ([< `b | `w | `l | `q ] as 'a) reg operand -> 'a reg operand -> text
(** [dst <- src ^ dst ]*)

val orb: [< [ `b ] source ] operand -> [< [ `b ] dest ] operand -> text
val orw: [< [ `w ] source ] operand -> [< [ `w ] dest ] operand -> text
val orl: [< [ `l ] source ] operand -> [< [ `l ] dest ] operand -> text
val orq: [< [ `q ] source ] operand -> [< [ `q ] dest ] operand -> text
val or_: ([< `b | `w | `l | `q ] as 'a) reg operand -> 'a reg operand -> text
(** [dst <- src | dst ]*)



val andb: [< [ `b ] source ] operand -> [< [ `b ] dest ] operand -> text
val andw: [< [ `w ] source ] operand -> [< [ `w ] dest ] operand -> text
val andl: [< [ `l ] source ] operand -> [< [ `l ] dest ] operand -> text
val andq: [< [ `q ] source ] operand -> [< [ `q ] dest ] operand -> text
val and_: ([< `b | `w | `l | `q ] as 'a) reg operand -> 'a reg operand -> text
(** [dst <- src & dst ]*)



(** {2 Décalages } *)

val salb: [ `imm ] operand -> [< [ `b ] dest ] operand -> text
val salw: [ `imm ] operand -> [< [ `w ] dest ] operand -> text
val sall: [ `imm ] operand -> [< [ `l ] dest ] operand -> text
val salq: [ `imm ] operand -> [< [ `q ] dest ] operand -> text
val sal: [ `imm ] operand -> ([< `b | `w | `l | `q ] as 'a) reg operand -> text
(** [dst <-  dst << src ]*)


val sarb: [ `imm ] operand -> [< [ `b ] dest ] operand -> text
val sarw: [ `imm ] operand -> [< [ `w ] dest ] operand -> text
val sarl: [ `imm ] operand -> [< [ `l ] dest ] operand -> text
val sarq: [ `imm ] operand -> [< [ `q ] dest ] operand -> text
val sar: [ `imm ] operand -> ([< `b | `w | `l | `q ] as 'a) reg operand -> text
(** [dst <-  dst >> src ], avec extension de signe *)


val shrb: [ `imm ] operand -> [< [ `b ] dest ] operand -> text
val shrw: [ `imm ] operand -> [< [ `w ] dest ] operand -> text
val shrl: [ `imm ] operand -> [< [ `l ] dest ] operand -> text
val shrq: [ `imm ] operand -> [< [ `q ] dest ] operand -> text
val shr: [ `imm ] operand -> ([< `b | `w | `l | `q ] as 'a) reg operand -> text
(** [dst <-  dst >> src ], avec extension par zéro *)

(** {2 Conditions } *)

val cmpb: [< [ `b ] source ] operand -> [< [ `b ] source ] operand -> text
val cmpw: [< [ `w ] source ] operand -> [< [ `w ] source ] operand -> text
val cmpl: [< [ `l ] source ] operand -> [< [ `l ] source ] operand -> text
val cmpq: [< [ `q ] source ] operand -> [< [ `q ] source ] operand -> text
val cmp: ([< `b | `w | `l | `q ] as 'a) reg operand -> ([< `b | `w | `l | `q ] as 'a) reg operand -> text

(** [cmp src dst] calcule [dst - src] (sans stocker le résultat) et positionne
    les flags de test en fonction du résultat. Attention au sens !*)

val testb: [< [ `b ] source ] operand -> [< [ `b ] source ] operand -> text
val testw: [< [ `w ] source ] operand -> [< [ `w ] source ] operand -> text
val testl: [< [ `l ] source ] operand -> [< [ `l ] source ] operand -> text
val testq: [< [ `q ] source ] operand -> [< [ `q ] source ] operand -> text
val test: ([< `b | `w | `l | `q ] as 'a) reg operand -> ([< `b | `w | `l | `q ] as 'a) reg operand -> text

(** [testx a b] effectue [a & b] (sans stocker le résultat) et positionne
    les flags de test en fonction du résultat. *)


val sete: [< [ `b ] dest ] operand -> text
val setne: [< [ `b ] dest ] operand -> text
val sets: [< [ `b ] dest ] operand -> text
val setns: [< [ `b ] dest ] operand -> text
val setg: [< [ `b ] dest ] operand -> text
val setge: [< [ `b ] dest ] operand -> text
val setl: [< [ `b ] dest ] operand -> text
val setle: [< [ `b ] dest ] operand -> text
val seta: [< [ `b ] dest ] operand -> text
val setae: [< [ `b ] dest ] operand -> text
val setb: [< [ `b ] dest ] operand -> text
val setbe: [< [ `b ] dest ] operand -> text
(** [set r] met l'octet du registre [r] à 1 si le flag considéré vaut 1 à 0 sinon *)

(** {2 Sauts } *)
val je: label -> text
val jne: label -> text
val js: label -> text
val jns: label -> text
val jg: label -> text
val jge: label -> text
val jl: label -> text
val jle: label -> text
val ja: label -> text
val jae: label -> text
val jb: label -> text
val jbe: label -> text
(** saute à l'étiquette donnée si le flag considéré est vaut 1, faut sinon *)

val jmp: label -> text
val jmp_star: [< [ `q ] source ] operand -> text
(** sauts inconditionnels *)

val call: label -> text
val call_star: [< [ `q ] source ] operand -> text
(** appel *)

val leave: text
val ret: text

(** {2 Opérations flottantes} *)
val movsd: [<  [ `d ] dest ] operand -> [<  [ `d ] dest ] operand -> text

(** déplacement. Il ne peut pas y avoir deux opérandes adresse *)

val cvtsi2sd: [< [ `l ] dest ] operand -> [ `d ] reg operand -> text
val cvtsi2sdq: [< [ `q ] dest ] operand -> [ `d ] reg operand -> text
val cvttsd2si: [< [ `d ] dest ] operand -> [ `l ] reg operand -> text
val cvttsd2siq: [< [ `d ] dest ] operand -> [ `q ] reg operand -> text
(** convertions depuis et vers des flottants *)

val addsd: [< [ `d ] dest ] operand -> [ `d ] reg operand -> text
val subsd: [< [ `d ] dest ] operand -> [ `d ] reg operand -> text
val mulsd: [< [ `d ] dest ] operand -> [ `d ] reg operand -> text
val divsd: [< [ `d ] dest ] operand -> [ `d ] reg operand -> text
val minsd: [< [ `d ] dest ] operand -> [ `d ] reg operand -> text
val maxsd: [< [ `d ] dest ] operand -> [ `d ] reg operand -> text
val sqrtsd: [< [ `d ] dest ] operand -> [ `d ] reg operand -> text
(** opérations arithmétiques *)

val ucomisd: [< [ `d ] dest ] operand -> [< [ `d ] dest ] operand -> text
(** comparaisons (équivalent de [cmp] pour les doubles) *)

(** {2 Pseudo-instructions}

    pour faciliter l'écriture du code, la bibliothèque fournit des instructions génériques
    qui testent la taille de leurs arguments (forcément des registres) pour choisir la bonne instruction.
    quand certaines combinaisons n'existent pas (movs_ ou movz_) des pseudo-instructions les simulant sont
    insérées
*)

val popd: [< [ `d ] dest ] operand -> text
val pushd: [< [ `d ] dest ] operand -> text
