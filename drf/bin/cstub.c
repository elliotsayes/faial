#include <caml/alloc.h>
#include <caml/mlvalues.h>
#include <caml/callback.h>
#include <string.h>

void __caml_init() {
    // Or pthread_once if you need it threadsafe.
    static int once = 0;

    if (once == 0) {
        // Fake an argv by prentending we're an executable `./ocaml_startup`.
        char* argv[] = {"ocaml_startup", NULL};

        // Initialize the OCaml runtime
        caml_startup(argv);

        once = 1;
    }
}

char* faial_drf_call_wgsl(const char* wgsl_json_str) {
  // Ensure the OCaml runtime is initialized before we invoke anything.
  __caml_init();

  CAMLparam0();

  // Fetch the function we registered via Callback.
  static const value* faial_drf_call_wgsl = NULL;
  if (faial_drf_call_wgsl == NULL)
    faial_drf_call_wgsl = caml_named_value("faial_drf_call_wgsl"); 

  // Convert C string to OCaml string
  value wgsl_json_ocaml_str = caml_copy_string(wgsl_json_str);

  // Invoke the function with the string argument
  CAMLlocal1(result);
  result = caml_callback_exn(*faial_drf_call_wgsl, wgsl_json_ocaml_str); 
  // assert(Tag_val(result) == String_tag);

  size_t result_len = caml_string_length(result);
  char* str_out = malloc(result_len);
  memcpy(str_out, String_val(result), result_len);

  // printf("faial_drf_call_wgsl: %s\n", str_out);

  CAMLreturnT(char*, str_out); 
}
