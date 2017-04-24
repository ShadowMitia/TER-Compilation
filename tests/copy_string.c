extern void * malloc(unsigned long p);
extern void free (void * p);
extern int printf();

void * copy_string(char * s) {
        unsigned long len;
        unsigned long i;
        char * start_s;
        char * d;
        start_s = s;
        for (len = 0; *s != 0; s++, len++);
        len ++; // pour \0
        d = malloc(len);

        for (i = 0; i < len; i++) {
                d[i] = start_s[i];
        };

        return d;

}

int main() {

        char * copy;
        char * orig;
        orig = "Mais ou est donc or ni car";
        copy = copy_string(orig);
        printf("%s\n", orig);
        printf("%s\n", copy);
        free(copy);

        return 0;

}
