type param_args = {
    mutable inline : bool
  }

let mk_compiler_param () = {
    inline = false;
  }

type fun_analyse = {
    mutable is_empty : bool;
    mutable is_rec : bool;
    mutable is_extern : bool;
  }


let mk_fun_analysis ext = {
    is_empty = false;
    is_rec = false;
    is_extern = ext;
  }
