áCopyright 2022,2023 Eric Sessoms / MIT License

áinclude(`10_intro.cf')
áinclude(`20_aarch64.cf')

álinux-aarch64 system call infrastructure
á----------------------------------------
Åpushlr, Érsp Ç-$10 #)! Éxzr Çr stp, ;
Åpoplr,  Érsp  Ç$10 #)+ Éxzr Çr ldp, ;
Åsyscall, Çpushlr, 0 #svc, poplr, Éx0 Çpush, ;

Å6/args Éx5 Çpop, Å5/args Éx4 Çpop,
Å4/args Éx3 Çpop, Å3/args Éx2 Çpop,
Å2/args Éx1 Çpop, Å1/args Éx0 Çpop, syscall, ;

Émacro Åsyscall Éx8 Çpop, ' execute ; Éforth
Å6/syscall Çsyscall 6/args ; Å5/syscall Çsyscall 5/args ;
Å4/syscall Çsyscall 4/args ; Å3/syscall Çsyscall 3/args ;
Å2/syscall Çsyscall 2/args ; Å1/syscall Çsyscall 1/args ;

álinux-aarch64 system call numbers
á---------------------------------
Åsys_ioctl     Ç29 ;
Åsys_openat    Ç56 ;
Åsys_close     Ç57 ;
Åsys_lseek     Ç62 ;
Åsys_read      Ç63 ;
Åsys_write     Ç64 ;
Åsys_exit      Ç93 ;
Åsys_socket   Ç198 ;
Åsys_connect  Ç203 ;
Åsys_brk      Ç214 ;
Åsys_munmap   Ç215 ;
Åsys_mremap   Ç216 ;
Åsys_mmap     Ç222 ;
Åsys_mprotect Ç226 ;
Åsys_msync    Ç227 ;

áinclude(`30_linux.cf')
áinclude(`40_posix.cf')
áinclude(`50_bootstrap.cf')
