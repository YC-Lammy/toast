mod common;

#[test]
fn test_block_scope() {
    common::run_test(include_str!("./test262/test/language/block-scope/leave/finally-block-let-declaration-only-shadows-outer-parameter-value-1.ts"));
    common::run_test(include_str!("./test262/test/language/block-scope/leave/finally-block-let-declaration-only-shadows-outer-parameter-value-2.ts"));
    common::run_test(include_str!("./test262/test/language/block-scope/leave/for-loop-block-let-declaration-only-shadows-outer-parameter-value-1.ts"));
    common::run_test(include_str!("./test262/test/language/block-scope/leave/for-loop-block-let-declaration-only-shadows-outer-parameter-value-2.ts"));
    common::run_test(include_str!("./test262/test/language/block-scope/leave/nested-block-let-declaration-only-shadows-outer-parameter-value-1.ts"));
    common::run_test(include_str!("./test262/test/language/block-scope/leave/nested-block-let-declaration-only-shadows-outer-parameter-value-2.ts"));
    common::run_test(include_str!("./test262/test/language/block-scope/leave/outermost-binding-updated-in-catch-block-nested-block-let-declaration-unseen-outside-of-block.ts"));
    common::run_test(include_str!("./test262/test/language/block-scope/leave/try-block-let-declaration-only-shadows-outer-parameter-value-1.ts"));
    common::run_test(include_str!("./test262/test/language/block-scope/leave/try-block-let-declaration-only-shadows-outer-parameter-value-2.ts"));
    common::run_test(include_str!(
        "./test262/test/language/block-scope/leave/verify-context-in-finally-block.ts"
    ));
    common::run_test(include_str!(
        "./test262/test/language/block-scope/leave/verify-context-in-for-loop-block.ts"
    ));
    common::run_test(include_str!(
        "./test262/test/language/block-scope/leave/verify-context-in-labelled-block.ts"
    ));
    common::run_test(include_str!(
        "./test262/test/language/block-scope/leave/verify-context-in-try-block.ts"
    ));
    common::run_test(include_str!(
        "./test262/test/language/block-scope/leave/x-after-break-to-label.ts"
    ));
    common::run_test(include_str!(
        "./test262/test/language/block-scope/leave/x-before-continue.ts"
    ));
}
