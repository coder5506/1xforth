remove-highlighter shared/colorforth
remove-hooks global colorforth

provide-module colorforth %{
   add-highlighter shared/colorforth group
   add-highlighter shared/colorforth/ regex \x81([^\x81-\x87]+) 1:red
   add-highlighter shared/colorforth/ regex \x82([^\x81-\x87]+) 1:green
   add-highlighter shared/colorforth/ regex \x83([^\x81-\x87]+) 1:yellow
   add-highlighter shared/colorforth/ regex \x86([^\x81-\x87]+) 1:cyan
   add-highlighter shared/colorforth/ regex \x87([^\x81-\x87]+) 1:black

   declare-option range-specs colorforth_tags

   define-command -hidden colorforth-update-ranges %{
      set-option window colorforth_tags %val{timestamp}
      evaluate-commands %sh{
         echo $kak_selections_desc | tr ' ' '\n' | while read selection; do
            echo "set-option -add window colorforth_tags '$selection|'" > $kak_command_fifo
         done
      }
   }

   define-command colorforth-hide-tags %{
      execute-keys \%s[\x81-\x87]<ret>:colorforth-update-ranges<ret>
   }

   define-command -hidden colorforth-configure-buffer %{
      set-option buffer extra_word_chars ! \" '#' $ \% & \' ( ) * + , - . / : \; < = > '?' @ [ '\' ] ^ _ ` { '|' } ~
   }

   define-command -hidden colorforth-configure-window %{
      add-highlighter window/colorforth ref colorforth
      add-highlighter window/ replace-ranges colorforth_tags

      hook -once -always window WinSetOption filetype=.* %{
         remove-highlighter window/colorforth
         add-highlighter window/ replace-ranges colorforth_tags
      }

      map window user r "i%sh{printf '\302\201'}<esc>"
      map window user g "i%sh{printf '\302\202'}<esc>"
      map window user y "i%sh{printf '\302\203'}<esc>"
      map window user c "i%sh{printf '\302\206'}<esc>"
      map window user w "i%sh{printf '\302\207'}<esc>"
   }
}

hook -group colorforth global BufSetOption filetype=colorforth %{
   require-module colorforth
   colorforth-configure-buffer
}

hook -group colorforth global WinSetOption filetype=colorforth %{
   require-module colorforth
   colorforth-configure-window
}

hook -group colorforth global BufCreate .*\.cf(\.m4)? %{
   set-option buffer filetype colorforth
}

hook -group colorforth global BufNewFile .*\.cf(\.m4)? %{
   set-option buffer filetype colorforth
}
