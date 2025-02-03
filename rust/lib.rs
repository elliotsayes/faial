use std::ffi::{CStr, CString};

use log::trace;
use parking_lot::Mutex;

unsafe extern "C" {
    unsafe fn faial_drf_call_wgsl(
        wgsl_json_str: *const ::std::os::raw::c_char,
    ) -> *const ::std::os::raw::c_char;
}

// Mutex to ensure that there is only one call to `faial_drf_call_wgsl` at a time
static FAIAL_DRF_CALL_WGSL: Mutex<()> = Mutex::new(());

pub fn faial_drf_call_wgsl_rust(wgsl_json_string: String) -> Result<String, String> {
    let wgsl_json_str = CString::new(wgsl_json_string).expect("CString::new failed");
    let wgsl_json_str_ptr = wgsl_json_str.as_ptr();

    let result: *const i8;
    let result_str: String;
    unsafe {
        let _guard = FAIAL_DRF_CALL_WGSL.lock();
        trace!("Calling faial_drf_call_wgsl: Begin");
        result = faial_drf_call_wgsl(wgsl_json_str_ptr);
        result_str = CStr::from_ptr(result)
            .to_str()
            .expect("CStr::to_str failed")
            .to_string();
        trace!("Calling faial_drf_call_wgsl: End");
    }

    Ok(result_str)
}

#[cfg(test)]
mod test {
    use super::*;

    #[test]
    fn it_works() {
        let wgsl_json = String::from("[{\"binding\":{\"binding\":0,\"group\":0},\"init\":null,\"kind\":\"GlobalDeclaration\",\"name\":\"data\",\"space\":{\"access\":{\"load\":true,\"store\":true},\"kind\":\"Storage\"},\"ty\":{\"inner\":{\"base\":{\"inner\":{\"kind\":\"Scalar\",\"value\":{\"kind\":\"Float\",\"width\":4}},\"name\":null},\"kind\":\"Array\",\"size\":{\"kind\":\"Dynamic\"}},\"name\":null}},{\"early_depth_test\":null,\"function\":{\"arguments\":[{\"binding\":{\"kind\":\"BuiltIn\",\"value\":\"WorkGroupId\"},\"kind\":\"FunctionArgument\",\"name\":\"blockIdx\",\"ty\":{\"inner\":{\"kind\":\"Vector\",\"scalar\":{\"kind\":\"Uint\",\"width\":4},\"size\":3},\"name\":null}},{\"binding\":{\"kind\":\"BuiltIn\",\"value\":\"NumWorkGroups\"},\"kind\":\"FunctionArgument\",\"name\":\"gridDim\",\"ty\":{\"inner\":{\"kind\":\"Vector\",\"scalar\":{\"kind\":\"Uint\",\"width\":4},\"size\":3},\"name\":null}},{\"binding\":{\"kind\":\"BuiltIn\",\"value\":\"LocalInvocationId\"},\"kind\":\"FunctionArgument\",\"name\":\"threadIdx\",\"ty\":{\"inner\":{\"kind\":\"Vector\",\"scalar\":{\"kind\":\"Uint\",\"width\":4},\"size\":3},\"name\":null}}],\"body\":[{\"accept\":[{\"kind\":\"Return\",\"value\":null}],\"condition\":{\"kind\":\"Binary\",\"left\":{\"kind\":\"Binary\",\"left\":{\"base\":{\"binding\":{\"kind\":\"BuiltIn\",\"value\":\"NumWorkGroups\"},\"kind\":\"FunctionArgument\",\"name\":\"gridDim\",\"ty\":{\"inner\":{\"kind\":\"Vector\",\"scalar\":{\"kind\":\"Uint\",\"width\":4},\"size\":3},\"name\":null}},\"index\":1,\"kind\":\"AccessIndex\",\"location\":{\"filename\":\"drf-saxpy.wgsl\",\"length\":9,\"line_number\":8,\"line_position\":7,\"offset\":282}},\"op\":\"NotEqual\",\"right\":{\"kind\":\"Literal\",\"value\":{\"kind\":\"u32\",\"value\":1}}},\"op\":\"LogicalOr\",\"right\":{\"kind\":\"Binary\",\"left\":{\"base\":{\"binding\":{\"kind\":\"BuiltIn\",\"value\":\"NumWorkGroups\"},\"kind\":\"FunctionArgument\",\"name\":\"gridDim\",\"ty\":{\"inner\":{\"kind\":\"Vector\",\"scalar\":{\"kind\":\"Uint\",\"width\":4},\"size\":3},\"name\":null}},\"index\":2,\"kind\":\"AccessIndex\",\"location\":{\"filename\":\"drf-saxpy.wgsl\",\"length\":9,\"line_number\":8,\"line_position\":25,\"offset\":300}},\"op\":\"NotEqual\",\"right\":{\"kind\":\"Literal\",\"value\":{\"kind\":\"u32\",\"value\":1}}}},\"kind\":\"If\",\"reject\":[]},{\"kind\":\"Store\",\"location\":{\"filename\":\"drf-saxpy.wgsl\",\"length\":23,\"line_number\":10,\"line_position\":3,\"offset\":371},\"pointer\":{\"base\":{\"kind\":\"GlobalVariable\",\"name\":\"data\",\"ty\":{\"inner\":{\"base\":{\"inner\":{\"kind\":\"Scalar\",\"value\":{\"kind\":\"Float\",\"width\":4}},\"name\":null},\"kind\":\"Array\",\"size\":{\"kind\":\"Dynamic\"}},\"name\":null}},\"index\":{\"kind\":\"Binary\",\"left\":{\"kind\":\"Binary\",\"left\":{\"base\":{\"binding\":{\"kind\":\"BuiltIn\",\"value\":\"WorkGroupId\"},\"kind\":\"FunctionArgument\",\"name\":\"blockIdx\",\"ty\":{\"inner\":{\"kind\":\"Vector\",\"scalar\":{\"kind\":\"Uint\",\"width\":4},\"size\":3},\"name\":null}},\"index\":0,\"kind\":\"AccessIndex\",\"location\":{\"filename\":\"drf-saxpy.wgsl\",\"length\":10,\"line_number\":9,\"line_position\":11,\"offset\":339}},\"op\":\"Multiply\",\"right\":{\"kind\":\"Literal\",\"value\":{\"kind\":\"u32\",\"value\":256}}},\"op\":\"Add\",\"right\":{\"base\":{\"binding\":{\"kind\":\"BuiltIn\",\"value\":\"LocalInvocationId\"},\"kind\":\"FunctionArgument\",\"name\":\"threadIdx\",\"ty\":{\"inner\":{\"kind\":\"Vector\",\"scalar\":{\"kind\":\"Uint\",\"width\":4},\"size\":3},\"name\":null}},\"index\":0,\"kind\":\"AccessIndex\",\"location\":{\"filename\":\"drf-saxpy.wgsl\",\"length\":11,\"line_number\":9,\"line_position\":28,\"offset\":356}}},\"kind\":\"Access\",\"location\":{\"filename\":\"drf-saxpy.wgsl\",\"length\":7,\"line_number\":10,\"line_position\":3,\"offset\":371}},\"value\":{\"kind\":\"Binary\",\"left\":{\"kind\":\"Load\",\"pointer\":{\"base\":{\"kind\":\"GlobalVariable\",\"name\":\"data\",\"ty\":{\"inner\":{\"base\":{\"inner\":{\"kind\":\"Scalar\",\"value\":{\"kind\":\"Float\",\"width\":4}},\"name\":null},\"kind\":\"Array\",\"size\":{\"kind\":\"Dynamic\"}},\"name\":null}},\"index\":{\"kind\":\"Binary\",\"left\":{\"kind\":\"Binary\",\"left\":{\"base\":{\"binding\":{\"kind\":\"BuiltIn\",\"value\":\"WorkGroupId\"},\"kind\":\"FunctionArgument\",\"name\":\"blockIdx\",\"ty\":{\"inner\":{\"kind\":\"Vector\",\"scalar\":{\"kind\":\"Uint\",\"width\":4},\"size\":3},\"name\":null}},\"index\":0,\"kind\":\"AccessIndex\",\"location\":{\"filename\":\"drf-saxpy.wgsl\",\"length\":10,\"line_number\":9,\"line_position\":11,\"offset\":339}},\"op\":\"Multiply\",\"right\":{\"kind\":\"Literal\",\"value\":{\"kind\":\"u32\",\"value\":256}}},\"op\":\"Add\",\"right\":{\"base\":{\"binding\":{\"kind\":\"BuiltIn\",\"value\":\"LocalInvocationId\"},\"kind\":\"FunctionArgument\",\"name\":\"threadIdx\",\"ty\":{\"inner\":{\"kind\":\"Vector\",\"scalar\":{\"kind\":\"Uint\",\"width\":4},\"size\":3},\"name\":null}},\"index\":0,\"kind\":\"AccessIndex\",\"location\":{\"filename\":\"drf-saxpy.wgsl\",\"length\":11,\"line_number\":9,\"line_position\":28,\"offset\":356}}},\"kind\":\"Access\",\"location\":{\"filename\":\"drf-saxpy.wgsl\",\"length\":7,\"line_number\":10,\"line_position\":13,\"offset\":381}}},\"op\":\"Multiply\",\"right\":{\"kind\":\"Literal\",\"value\":{\"kind\":\"f32\",\"value\":2.0}}}},{\"kind\":\"Return\",\"value\":null}],\"kind\":\"Function\",\"locals\":[],\"name\":\"computeSomething\",\"result\":null},\"kind\":\"EntryPoint\",\"name\":\"computeSomething\",\"stage\":\"Compute\",\"workgroup_size\":[256,1,1]}]");
        let res = faial_drf_call_wgsl_rust(wgsl_json);
        assert!(res.is_ok());
    }
}
