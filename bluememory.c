
// mmap/mprotect/munmap (or equivalent) wrappers
// for allocating pages and changing their protections

#ifdef _WIN32
#  include <windows.h>
#else
#  include <sys/mman.h>
#endif

#ifdef _WIN32
/*
void * bluemoon_mmap(int sz, int align) {
	return VirtualAlloc(0, sz, MEM_COMMIT | MEM_RESERVE, PAGE_EXECUTE_READWRITE);
}

int bluemoon_mprotect_rw(void * p, int sz) {
	DWORD _;
	//return VirtualProtect(p, sz, PAGE_EXECUTE_READ, &_);
}
*/
//int bluemoon_mprotect_rwx

// munmap

#else

#if __APPLE__ && __MACH__
#  define MAP_ANONYMOUS MAP_ANON
#endif

void * bluemoon_mmap(size_t sz) {
	return mmap(0, sz, PROT_READ | PROT_WRITE, MAP_ANONYMOUS, -1, 0);
}

int bluemoon_mprotect_rw(void * p, size_t sz) {
	return mprotect(p, sz, PROT_READ | PROT_WRITE);
}

int bluemoon_mprotect_rwx(void * p, size_t sz) {
	return mprotect(p, sz, PROT_READ | PROT_WRITE | PROT_EXEC);
}

int bluemoon_munmap(void * p, size_t sz) {
	return munmap(p, sz);
}

#endif

