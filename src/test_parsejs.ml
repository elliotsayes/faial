open OUnit2
open Parsejs
open Testutil

let shared_attr : j_object =
  let open Yojson.Basic in
  [
    "kind", `String "AnnotateAttr";
    "spelling", `String "annotate";
    "value", `String " __attribute__((annotate(\"shared\")))"
  ]

let shared_array : j_object =
  let open Yojson.Basic in
  [
    "inner", `List [`Assoc shared_attr];
    "isUsed", `Bool true;
    "kind", `String "VarDecl";
    "name", `String "smem";
    "type", `Assoc ["qualType", `String "int [128]"];
  ]

let tests = "tests" >::: [
  "ojson" >:: (fun _ ->
    let open Ojson in
    assert_bool "is_shared_attr" (is_shared_attr shared_attr);
    assert_bool "has_shared_attr" (has_shared_attr shared_array);
    assert_bool "is_shared_array" (is_shared_array shared_array);
    match get_type shared_array with
    | None -> assert_bool "could not get type" false
    | Some ty ->
    assert_equal (Ctype.to_string ty) "int [128]";
    let a = mk_array SharedMemory ty in
    assert_equal a.array_type ["int"];
    assert_equal a.array_size [128];
  );
  "parse_position" >:: (fun _ ->
    let s = "
      {
        \"line\": 386,
        \"col\": 123
      }
    " in
    let open Yojson.Basic in
    let j = from_string s in
    let p = parse_position.run j in
    assert_position_equal p {pos_line = 386; pos_column = 123; pos_filename=""};
    let s = "
      {
        \"line\": 386,
        \"col\": 123,
        \"file\": \"foo\"
      }
    " in
    let j = from_string s in
    let p = parse_position.run j in
    assert_position_equal p {pos_line = 386; pos_column = 123; pos_filename="foo"};
    let s = "{
        \"spellingLoc\": {
          \"line\": 386,
          \"col\": 123
        },
        \"expansionLoc\": {
          \"line\": 391,
          \"col\": 1
        }
    }" in
    let j = from_string s in
    let p = parse_position.run j in
    assert_position_equal p {pos_line = 391; pos_column = 1; pos_filename=""};
    ()
  );
  "parse_loc" >:: (fun _ ->
    let s = "{\"begin\": {\"line\": 20, \"col\": 30}, \"end\": {\"line\": 22, \"col\": 32}}"
    in
    let open Yojson.Basic in
    let j = from_string s in
    assert (parse_loc.is_valid j);
    let p = parse_loc.run j in
    assert_location_equal p {
      loc_start = {pos_line = 20; pos_column = 30; pos_filename = ""};
      loc_end = {
        pos_line = 22;
        pos_column = 32;
        pos_filename = ""
      }
    };
    ()
  );
  "parse_decl1" >:: (fun _ ->
    let s = "{
      \"kind\" : \"BinaryOperator\",
      \"lhs\" : {
          \"kind\" : \"VarDecl\",
          \"name\" : \"mem_ai\",
          \"type\" : {
            \"qualType\" : \"int\"
          }
      },
      \"opcode\" : \"=\",
      \"rhs\" : {
          \"kind\" : \"BinaryOperator\",
          \"lhs\" : {
            \"kind\" : \"ParmVarDecl\",
            \"name\" : \"baseIndex\",
            \"type\" : {
                \"qualType\" : \"int\"
            }
          },
          \"opcode\" : \"+\",
          \"rhs\" : {
            \"kind\" : \"VarDecl\",
            \"name\" : \"threadIdx.x\",
            \"type\" : {
                \"qualType\" : \"const unsigned int\"
            }
          },
          \"type\" : {
            \"qualType\" : \"unsigned int\"
          }
      },
      \"type\" : {
          \"qualType\" : \"int\"
      }
    }
    "
    in
    let open Yojson.Basic in
    let j = from_string s in
    let s = parse_stmt.run j in
    match s with
    | Decl _ -> ()
    | _ -> assert false
  );
  "parse_var" >:: (fun _ ->
    let s = "
          {
            \"init\": \"c\",
            \"inner\": [
              {
                \"kind\": \"VarDecl\",
                \"name\": \"n\",
                \"type\": {\"qualType\": \"const int\"}
              }
            ],
            \"isUsed\": true,
            \"kind\": \"VarDecl\",
            \"name\": \"id\",
            \"type\": {\"qualType\": \"int\"}
          }"
    in
    let open Yojson.Basic in
    let j = from_string s in
    let v = parse_var.run j in
    assert_string_equal "id" (Exp.var_name v)
  );
  "parse_decl2" >:: (fun _ ->
    let s = "
      {
        \"inner\": [
          {
            \"init\": \"c\",
            \"inner\": [
              {
                \"kind\": \"VarDecl\",
                \"name\": \"n\",
                \"type\": {\"qualType\": \"const int\"}
              }
            ],
            \"isUsed\": true,
            \"kind\": \"VarDecl\",
            \"name\": \"id\",
            \"type\": {\"qualType\": \"int\"}
          }
        ],
        \"kind\": \"DeclStmt\"
      }" in
    let open Yojson.Basic in
    let j = from_string s in
    let s = parse_stmt.run j in
    match s with
    | Block [Decl _] -> ()
    | _ -> assert false
  );
  "parse_multi_decl" >:: (fun _ ->
    let s = "
    {\"inner\": [
      {\"init\": \"call\",
       \"inner\":
        [
          {\"kind\": \"VarDecl\",
          \"name\": \"threadIdx.x\",
          \"type\": {\"qualType\": \"const unsigned int\"}
          }
        ],
       \"isUsed\": true,
       \"kind\": \"VarDecl\",
       \"name\": \"tid\",
       \"type\": {\"desugaredQualType\": \"unsigned int\", \"qualType\": \"uint\"}},
      {\"init\": \"call\",
       \"inner\": [
        {\"kind\": \"VarDecl\",
         \"name\": \"blockIdx.x\",
         \"type\": {\"qualType\": \"const unsigned int\"}
        }],
       \"isUsed\": true,
       \"kind\": \"VarDecl\",
       \"name\": \"x\",
       \"type\": {\"desugaredQualType\": \"unsigned int\", \"qualType\": \"uint\"}},
      {\"init\": \"call\",
       \"inner\": [
        {\"kind\": \"VarDecl\",
         \"name\": \"blockIdx.y\",
         \"type\": {\"qualType\": \"const unsigned int\"}}
        ],
       \"isUsed\": true,
       \"kind\": \"VarDecl\",
       \"name\": \"y\",
       \"type\": {\"desugaredQualType\": \"unsigned int\", \"qualType\": \"uint\"}
      }
      ], \"kind\": \"DeclStmt\"}
    " in
    let open Yojson.Basic in
    let j = from_string s in
    let s = parse_stmt.run j in
    match s with
    | Block [Decl _; Decl _; Decl _]  -> ()
    | _ -> assert false
  );
  "parse_var" >:: (fun _ ->
      let s =
      "{
          \"kind\": \"ParmVarDecl\",
          \"loc\": {
            \"spellingLoc\": {
              \"line\": 386,
              \"col\": 133
            },
            \"expansionLoc\": {
              \"line\": 391,
              \"col\": 1
            }
          },
          \"range\": {
            \"begin\": {
              \"spellingLoc\": {
                \"line\": 386,
                \"col\": 126
              },
              \"expansionLoc\": {
                \"line\": 391,
                \"col\": 1
              }
            },
            \"end\": {
              \"spellingLoc\": {
                \"line\": 386,
                \"col\": 133
              },
              \"expansionLoc\": {
                \"line\": 391,
                \"col\": 1
              }
            }
          },
          \"name\": \"t\",
          \"type\": {
            \"desugaredQualType\": \"texture<short2, 3, cudaReadModeNormalizedFloat>\",
            \"qualType\": \"texture<short2, 3, cudaReadModeNormalizedFloat>\"
          }
        }
        "
    in
    let open Yojson.Basic in
    let j = from_string s in
    assert (parse_var.is_valid j);
    let v = parse_var.run j in
    assert_var_equal v (LocVariable (
      {
      loc_start = {pos_line = 391; pos_column = 1; pos_filename = ""};
      loc_end = {pos_line = 391; pos_column = 1; pos_filename = ""};
      }, "t"));
      let s =
      "{
          \"kind\": \"ParmVarDecl\",
          \"name\": \"t\"
        }
        "
    in
    let j = from_string s in
    assert (parse_var.is_valid j);
    let v = parse_var.run j in
    assert_var_equal v (Variable "t");
    ()
  );
  "get_kind_res" >:: (fun _ ->
    let open Yojson.Basic in
    [
        ("{\"kind\":\"ParmVarDecl\",\"name\":\"gridDim.x\",\"type\":{\"qualType\":\"const unsigned int\"},\"isUsed\":true}",
        "ParmVarDecl");
    ] |>
    List.iter (fun (s, k) ->
        let j = from_string s in
        let expect_kind k j =
            match get_kind_res j with
            | Result.Ok x -> assert_equal k x
            | Result.Error x -> assert_bool x false
        in
    expect_kind k j
    )
  );
  "access_stmt" >:: (fun _ ->
    let s = "
      {
          \"index\" : [{
               \"isUsed\" : true,
               \"kind\" : \"ParmVarDecl\",
               \"name\" : \"blockDim.x\",
               \"type\" : {
                  \"qualType\" : \"const unsigned int\"
               }
            }],
          \"kind\" : \"AccessStmt\",
          \"location\" : {
              \"kind\" : \"VarDecl\",
              \"name\" : \"s_key\",
              \"range\" : {
                \"begin\" : {
                    \"col\" : 5,
                    \"file\" : \"bitonicMergeShared.cu\",
                    \"line\" : 32
                },
                \"end\" : {
                    \"col\" : 48,
                    \"file\" : \"bitonicMergeShared.cu\",
                    \"line\" : 32
                }
              },
              \"type\" : {
                \"qualType\" : \"uint [1024]\"
              }
          },
          \"mode\" : \"rw\"
      }
    " in
    let open Yojson.Basic in
    let j = from_string s in
    assert (parse_stmt.is_valid j);
    let i = parse_stmt.run j in
    (match i with
    | Acc _ -> ()
    | _ -> assert false
    );
    ()
  );
]

let _ = run_test_tt_main tests
