// mult.c

#include <stdio.h>
#include <stdbool.h>

bool odd(int n) { return n & 0x1; }
int half(int n) { return n >> 1; }

int mult_acc4(int r, int n, int a) {
	while (true) {
		if (odd(n)) {
			r = r + a;
			if (n == 1) return r;
		}
		n = half(n);
		a = a + a;
	}
}

int multiply2(int n, int a) {
	if (n == 1) return a;
	return mult_acc4(a, n - 1, a);
}

int multiply4(int n, int a) {
	while (!odd(n)) {
		a = a + a;
		n = half(n);
	}
	if (n == 1) return a;
    // even(n − 1) =⇒ n − 1 ̸= 1
	return mult_acc4(a, half(n - 1), a + a);
}
