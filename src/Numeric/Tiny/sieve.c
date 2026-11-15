// sieve.c

template <RandomAccessIterator I, Integer N>
	void mark_sieve(I first, I last, N factor) {
	// assert (first != last)
	*first = false;
	while (last - first > factor) {
		first  = first + factor;
		*first = false;
	}
}

template <RandomAccessIterator I, Integer N>
	void sift0(I first, N n) {
	std::fill(first, first + n, true);
	N i(0);
	N index_square(3);
	while (index_square < n) {
		// invariant: index_square = 2i^2 + 6i + 3
		if (first[i]) {
			mark_sieve ( first + index_square
					   , first + n	 // last
					   , i + i + 3 ); // factor
		}
		++i;
		index_square = 2*i*(i + 3) + 3;
	}
}
