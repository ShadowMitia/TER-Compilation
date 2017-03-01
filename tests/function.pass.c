int myfunc();
void myprocedure() {}
char myfuncwithargs(int a);
char myfuncwithargs2(int a, int b);

int test(int a, int b, int c) {
  if (1) {}
  if (1) {} else {}
  if (1) {{}}
  while (1) {}
  return 42;
}

int test2() {
  1 + 1;
  1 - 1;
  1/1;
  1*1;
  1&&1;
  1||1;
  1!=1;
  1<1;
  1>1;
  1<=1;
  1>=1;
}

/*
void test2() {
  int* test;
  test;
  test[0];
  return;
}


void test3() {

  int t1;
  int t2;
  t1 = t2;

  test2();

  ++t1;
  --t1;
  t1++;
  t1--;
  &t1;
  !t1;
  -t1;
  +t1;

  t1 + t2;
  t1 - t2;
  t1 * t2;
  t1 / t2;
  t1 % t2;
  t1 && t2;
  t1 || t2;
  t1 == t2;
  t1 != t2;
  t1 < t2;
  t1 > t2;
  t1 <= t2;
  t1 >= t2;

  sizeof(void);

  (void)t1;
  (t1);
}
*/
