//extern int printf(char * s, int i);
extern int puts(char *s);
extern int printf();


int main() {
        int i;
        for (i = 0; i < 10; i++)
                printf("%d\n",  i);

        return 0;
}
