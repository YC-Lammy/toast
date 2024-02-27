use native_ts_hir::ast::format::Formatter;
use native_ts_hir::transform::Transformer;

#[test]
fn binary_search() {
    let s = "binarySearch([], 0);
    function binarySearch(arr: number[], x: number): number
    {    
        let l = 0;
        let r = arr.length - 1;
        let mid: number;
        while (r >= l) {
             mid = l + (r - l) / 2;
      
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

    for (_id, module) in m.modules {
        let mut t = Transformer::new();

        let re = t.transform_module(&module.module).expect("parse error");
        let mut formatter = Formatter::new(&re.table);
        formatter.format_module(&re);

        let formated = formatter.emit_string();
        println!("{}", formated);
    }
}
