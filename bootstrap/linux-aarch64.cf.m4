�Copyright 2022,2023 Eric Sessoms / MIT License

�include(`10_intro.cf')
�include(`20_aarch64.cf')

�linux-aarch64 system call infrastructure
�----------------------------------------
�pushlr, �rsp �-$10 #)! �xzr �r stp, ;
�poplr,  �rsp  �$10 #)+ �xzr �r ldp, ;
�syscall, �pushlr, 0 #svc, poplr, �x0 �push, ;

�6/args �x5 �pop, �5/args �x4 �pop,
�4/args �x3 �pop, �3/args �x2 �pop,
�2/args �x1 �pop, �1/args �x0 �pop, syscall, ;

�macro �syscall �x8 �pop, ' execute ; �forth
�6/syscall �syscall 6/args ; �5/syscall �syscall 5/args ;
�4/syscall �syscall 4/args ; �3/syscall �syscall 3/args ;
�2/syscall �syscall 2/args ; �1/syscall �syscall 1/args ;

�linux-aarch64 system call numbers
�---------------------------------
�sys_ioctl     �29 ;
�sys_openat    �56 ;
�sys_close     �57 ;
�sys_lseek     �62 ;
�sys_read      �63 ;
�sys_write     �64 ;
�sys_exit      �93 ;
�sys_socket   �198 ;
�sys_connect  �203 ;
�sys_brk      �214 ;
�sys_munmap   �215 ;
�sys_mremap   �216 ;
�sys_mmap     �222 ;
�sys_mprotect �226 ;
�sys_msync    �227 ;

�include(`30_linux.cf')
�include(`40_posix.cf')
�include(`50_bootstrap.cf')
