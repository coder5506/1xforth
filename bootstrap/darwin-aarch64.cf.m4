áCopyright 2022,2023 Eric Sessoms / MIT License

áinclude(`10_intro.cf')
áinclude(`20_aarch64.cf')

ádarwin-aarch64 system call infrastructure
á-----------------------------------------
Åpushlr, Érsp Ç-$10 #)! Éxzr Çr stp, ;
Åpoplr,  Érsp  Ç$10 #)+ Éxzr Çr ldp, ;
Åsyscall, Çpushlr, $80 #svc, poplr, Éx0 Çpush, ;

Å6/args Éx5 Çpop, Å5/args Éx4 Çpop,
Å4/args Éx3 Çpop, Å3/args Éx2 Çpop,
Å2/args Éx1 Çpop, Å1/args Éx0 Çpop, syscall, ;

Émacro Åsyscall Éx16 Çpop, ' execute ; Éforth
Å6/syscall Çsyscall 6/args ; Å5/syscall Çsyscall 5/args ;
Å4/syscall Çsyscall 4/args ; Å3/syscall Çsyscall 3/args ;
Å2/syscall Çsyscall 2/args ; Å1/syscall Çsyscall 1/args ;

ádarwin-aarch64 system call numbers
á----------------------------------
Åsys_exit      Ç1 ;
Åsys_read      Ç3 ;
Åsys_write     Ç4 ;
Åsys_close     Ç6 ;
Åsys_ioctl    Ç54 ;
Åsys_msync    Ç65 ;
Åsys_munmap   Ç73 ;
Åsys_mprotect Ç74 ;
Åsys_socket   Ç97 ;
Åsys_connect  Ç98 ;
Åsys_mmap    Ç197 ;
Åsys_lseek   Ç199 ;
Åsys_openat  Ç463 ;

áinclude(`30_darwin.cf')
áinclude(`35_bsd.cf')
áinclude(`40_posix.cf')
áinclude(`50_bootstrap.cf')
