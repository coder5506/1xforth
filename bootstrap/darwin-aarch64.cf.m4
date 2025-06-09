�Copyright 2022,2023 Eric Sessoms / MIT License

�include(`10_intro.cf')
�include(`20_aarch64.cf')

�darwin-aarch64 system call infrastructure
�-----------------------------------------
�pushlr, �rsp �-$10 #)! �xzr �r stp, ;
�poplr,  �rsp  �$10 #)+ �xzr �r ldp, ;
�syscall, �pushlr, $80 #svc, poplr, �x0 �push, ;

�6/args �x5 �pop, �5/args �x4 �pop,
�4/args �x3 �pop, �3/args �x2 �pop,
�2/args �x1 �pop, �1/args �x0 �pop, syscall, ;

�macro �syscall �x16 �pop, ' execute ; �forth
�6/syscall �syscall 6/args ; �5/syscall �syscall 5/args ;
�4/syscall �syscall 4/args ; �3/syscall �syscall 3/args ;
�2/syscall �syscall 2/args ; �1/syscall �syscall 1/args ;

�darwin-aarch64 system call numbers
�----------------------------------
�sys_exit      �1 ;
�sys_read      �3 ;
�sys_write     �4 ;
�sys_close     �6 ;
�sys_ioctl    �54 ;
�sys_msync    �65 ;
�sys_munmap   �73 ;
�sys_mprotect �74 ;
�sys_socket   �97 ;
�sys_connect  �98 ;
�sys_mmap    �197 ;
�sys_lseek   �199 ;
�sys_openat  �463 ;

�include(`30_darwin.cf')
�include(`35_bsd.cf')
�include(`40_posix.cf')
�include(`50_bootstrap.cf')
