struct S {
  int a;
  int b;
  int c;
};

struct S functionStruct() {
  struct S s;
  s.a;
  /*struct S* sp;
    sp->a;*/
  return s;
}
