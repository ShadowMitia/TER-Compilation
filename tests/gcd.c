extern int printf();
extern void exit(int x);
int main() {
            int a;
            int b;
            int x;
            int y;
            a = 2000;
            b = 2000;
            x = a;
            y = b;
      for (;b != 0;)
      {
              int t;
              t = b;
              b = a % b;
              a = t;
      }
      printf("Le pgcd de %d et %d est %d\n", x, y, a);

      exit(0);
      return 0;
}
