extern int putchar(int c);
extern int printf();
extern void exit(int x);

int limit;

int iter(int n, double a, double b, double xn, double yn) {
        double xn2;
        double yn2;

  if (n == limit) return 1;
  xn2 = xn * xn;
  yn2 = yn * yn;
  if ((xn2 + yn2) > 4.0) return 0;
  return iter(n+1, a, b, a + (xn2 - yn2),
              b+ (2.0 *  xn * yn));
}

int inside(double x, double y) {
        return iter(0, x, y, 0.0, 0.0);
}

void run(int steps) {
        double xmin;
        double xmax;
        double deltax;
        double ymin;
        double ymax;
        double deltay;
        int i;
        xmin = -2.0;
        xmax = 1.0;
        deltax = (xmax - xmin) / (2 * steps);
        ymin = -1.0;
        ymax = 1.0;
        deltay = (ymax - ymin) /  steps;
        printf("xmin: %f, xmax: %f\n", xmin, xmax);
        for (i = 0; i < steps; i++) {
                double y;
                int j;
                y = ymin + i * deltay;
                for (j = 0; j < 2 * steps; j = j + 1) {
                        double x;
                        int r;
                        x = xmin + j * deltax;
                        r = inside (x, y);
                        if (r)
                                printf("%c", '0');
                        else
                                printf("%c", '1');
                }
                printf("%s", "\n");
        }
}

int main() {
        limit = 100;
        run(30);
        exit(0);
        return 0;
}
