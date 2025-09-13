Possible requests (and associated responses) (in `JSON` format) when using the compiler in "server" mode (`rfsmc -server_mode ...`)

- REQUEST: `{"version": ""}` : get compiler version
  RESPONSE: `{"version": "<version>"}`

- REQUEST: `{"check": <fragment>}` : check program fragment
  where `fragment`:=`{"inps:<iovs>","outps":<iovs>,"vars"=<iovs>,"obj"=<fragment_obj>}` 
  and   `iovs`:=`[<iov>,...,<iov>]`
  and   `iov`:=`{"id":<ident>,"type":<type>}`
  and   `type`:= `int|bool|event`
  and `fragment_obj` := `<guard>|<action>|<sval>`
  and `guard` := `guard <bool_expr>`
  and `action` := `action <ident>:=<exrp>`
  and `sval` := `sval <state_id>:=<const_expr>`
  Example:
  `{"check":"{inps":[{"id":"h","type":"clk"},{"id":"e","type":"int"}],"outps":[{"id":"s","type":"bool"}],"vars":[{"id":"k","type":"int"}],"obj":"guarde=1"}"}`
  RESPONSE: 
  - `{"ok"; ""}`
  - `{"err"; <err_msg>}`

- REQUEST: `{"compile": {"args:[...]}}<args>}` : compile program
  RESPONSE:
  - `{"ok": [<list_of_output_files>]}`
  - `{"err": <compile_err>}`

