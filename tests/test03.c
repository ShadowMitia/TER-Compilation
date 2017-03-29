extern int printf();

int f(int x) {


        return x;
}


long g(long x) {

        return x;
}

int main() {

        printf("%d\n", f(1234));
        printf("%ld\n", g(1234l));
        return 0;

}
