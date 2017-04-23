type param_args = {
    mutable inline : bool
  }

let mk_compiler_param () = {
    inline = false;
  }

type (*('is_rec, 'is_empty)*) fun_analyse = {
    mutable is_empty : bool;
    mutable is_rec : bool;
  }


let mk_fun_analysis () = {
    is_empty = false;
    is_rec = false
  }
