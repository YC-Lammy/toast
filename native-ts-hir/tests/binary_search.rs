use std::collections::HashMap;

use native_ts_hir::ast::format::Formatter;
use native_ts_hir::ast::Program;
use native_ts_hir::interpreter::Interpreter;
use native_ts_hir::transform::Transformer;

#[test]
fn binary_search() {
    let s = "binarySearch([0, 8, 9, 10, 11, 12, 13, 14], 12);
    function round(i: number): number{
        let rem = i % 1;
        let n = i - rem;

        if (rem >= 0.5){
            return n + 1
        }
        return n
    }
    function binarySearch(arr: number[], x: number): number
    {
        let l = 0;
        let r = arr.length - 1;
        let mid: number;
        while (r >= l) {
             mid = l + round((r - l) / 2);
      
            // If the element is present at the middle
            // itself
            if (arr[mid] == x)
                return mid;
      
            // If element is smaller than mid, then
            // it can only be present in left subarray
            if (arr[mid] > x)
                r = mid - 1;

            // Else the element can only be present
            // in right subarray
            else
                l = mid + 1;
        }
      
        // We reach here when element is not
        // present in array
        return -1;
    }";

    let parser = native_ts_parser::Parser::new();
    let m = parser
        .parse_str("test".to_string(), s.to_string())
        .expect("parse failed");

    for (id, module) in m.modules {
        let mut t = Transformer::new();

        let re = t
            .transform_module(&module.module, vec![])
            .expect("parse error");
        let mut formatter = Formatter::new(&re.table);
        formatter.format_module(&re);

        let formated = formatter.emit_string();
        println!("{}", formated);

        let intpr = Interpreter::new();

        let id = unsafe { core::mem::transmute(id) };

        let re = intpr.run(&Program {
            entry: id,
            modules: HashMap::from_iter([(id, re)]),
        });

        println!("{:#?}", re);
    }
}
