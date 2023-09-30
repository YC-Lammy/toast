
@global_string = global [16 x i8] c"\07\00\00\00\00\00\00\00rjeifie\00"

declare double @RT_entry()

declare void @RT_exit()

declare i1 @RT_has_exception()

declare void @RT_set_exception(double)

declare double @RT_get_exception()

declare void @RT_throw(double)

declare void @RT_enter_try()

declare void @RT_exit_try()

declare ptr @RT_gc_malloc(i32)

declare double @RT_await(double)

declare double @RT_yield(double)

declare ptr @RT_create_for_in_iter(double)

declare ptr @RT_create_for_of_iter(double)

declare ptr @RT_create_async_iter(double)

declare double @RT_iter_next(ptr)

declare i1 @RT_iter_done(ptr)

declare void @RT_drop_iter(ptr)

declare double @RT_call(double, double, i32, ptr)

declare double @RT_call_varargs(double, double, i32, ptr)

declare double @RT_call_static_vararg(ptr, ptr, double, i32, ptr)

declare double @RT_new_call(double, i32, ptr)

declare double @RT_create_regexp(ptr, i32, ptr, i32)

declare double @RT_create_array(i32)

declare void @RT_array_push(double, double)

declare double @RT_create_object()

declare double @RT_object_get(double, i64)

declare double @RT_object_get_index(double, i32)

define double @dc0b172e-2a36-4322-b32d-b7ee89059af9(double %0, ptr %1, i32 %2, double %3, double %4, double %5, ...) {
entry:
  %ACC = alloca double, align 8
  %ERROR = alloca double, align 8
  %THIS = alloca double, align 8
  store double %0, ptr %THIS, align 8
  store double 0x7FFB000000000000, ptr %ACC, align 8
  br label %return_blcok

return_blcok:                                     ; preds = %entry
  %load_acc = load double, ptr %ACC, align 8
  ret double %load_acc
}

define double @ddb7614e-f59b-48a6-83a0-f1acb20accef(double %0, ptr %1, i32 %2, double %3, double %4, double %5, ...) {
entry:
  %ACC = alloca double, align 8
  %ERROR = alloca double, align 8
  %THIS = alloca double, align 8
  store double %0, ptr %THIS, align 8
  %alloca = alloca double, align 8
  %alloca1 = alloca double, align 8
  %create_array = call double @RT_create_array(i32 3)
  store double %create_array, ptr %ACC, align 8
  %load_acc = load double, ptr %ACC, align 8
  %alloca2 = alloca double, align 8
  store double %load_acc, ptr %alloca2, align 8
  store double 0.000000e+00, ptr %ACC, align 8
  %load_temp = load double, ptr %alloca2, align 8
  %load_acc3 = load double, ptr %ACC, align 8
  call void @RT_array_push(double %load_temp, double %load_acc3)
  store double 9.000000e+00, ptr %ACC, align 8
  %load_temp4 = load double, ptr %alloca2, align 8
  %load_acc5 = load double, ptr %ACC, align 8
  call void @RT_array_push(double %load_temp4, double %load_acc5)
  store double 7.000000e+00, ptr %ACC, align 8
  %load_temp6 = load double, ptr %alloca2, align 8
  %load_acc7 = load double, ptr %ACC, align 8
  call void @RT_array_push(double %load_temp6, double %load_acc7)
  %load_temp8 = load double, ptr %alloca2, align 8
  store double %load_temp8, ptr %ACC, align 8
  %destroy_temp = load double, ptr %alloca2, align 8
  %load_acc9 = load double, ptr %ACC, align 8
  store double %load_acc9, ptr %alloca, align 8
  %load_variable = load double, ptr %alloca, align 8
  store double %load_variable, ptr %ACC, align 8
  %load_acc10 = load double, ptr %ACC, align 8
  %create_for_of_iter = call ptr @RT_create_for_of_iter(double %load_acc10)
  br label %loop_body

return_blcok:                                     ; preds = %if_exit86
  %load_acc100 = load double, ptr %ACC, align 8
  ret double %load_acc100

loop_body:                                        ; preds = %loop_station, %entry
  %iter_done = call i1 @RT_iter_done(ptr %create_for_of_iter)
  br i1 %iter_done, label %loop_exit, label %exit_block

loop_station:                                     ; preds = %block_exit
  br label %loop_body

loop_exit:                                        ; preds = %loop_body
  call void @RT_drop_iter(ptr %create_for_of_iter)
  %load_variable20 = load double, ptr %alloca, align 8
  store double %load_variable20, ptr %ACC, align 8
  %load_acc21 = load double, ptr %ACC, align 8
  %create_for_in_iter = call ptr @RT_create_for_in_iter(double %load_acc21)
  br label %loop_body22

exit_block:                                       ; preds = %loop_body
  %iter_next = call double @RT_iter_next(ptr %create_for_of_iter)
  store double %iter_next, ptr %ACC, align 8
  %alloca11 = alloca double, align 8
  %load_acc12 = load double, ptr %ACC, align 8
  store double %load_acc12, ptr %alloca11, align 8
  %load_variable13 = load double, ptr %alloca11, align 8
  store double %load_variable13, ptr %ACC, align 8
  %load_acc14 = load double, ptr %ACC, align 8
  %alloca15 = alloca double, align 8
  store double %load_acc14, ptr %alloca15, align 8
  store double 0x7FFB000000000000, ptr %ACC, align 8
  %load_acc16 = load double, ptr %ACC, align 8
  %alloca17 = alloca double, align 8
  store double %load_acc16, ptr %alloca17, align 8
  br label %block_exit

block_exit:                                       ; preds = %exit_block
  %destroy_temp18 = load double, ptr %alloca15, align 8
  %destroy_temp19 = load double, ptr %alloca17, align 8
  br label %loop_station

loop_body22:                                      ; preds = %loop_station23, %loop_exit
  %iter_done26 = call i1 @RT_iter_done(ptr %create_for_in_iter)
  br i1 %iter_done26, label %loop_exit24, label %exit_block25

loop_station23:                                   ; preds = %block_exit35
  br label %loop_body22

loop_exit24:                                      ; preds = %loop_body22
  call void @RT_drop_iter(ptr %create_for_in_iter)
  store double 8.800000e+01, ptr %ACC, align 8
  %load_acc38 = load double, ptr %ACC, align 8
  store double %load_acc38, ptr %alloca1, align 8
  br label %loop_body39

exit_block25:                                     ; preds = %loop_body22
  %iter_next27 = call double @RT_iter_next(ptr %create_for_in_iter)
  store double %iter_next27, ptr %ACC, align 8
  %alloca28 = alloca double, align 8
  %load_acc29 = load double, ptr %ACC, align 8
  store double %load_acc29, ptr %alloca28, align 8
  %load_variable30 = load double, ptr %alloca28, align 8
  store double %load_variable30, ptr %ACC, align 8
  %load_acc31 = load double, ptr %ACC, align 8
  %alloca32 = alloca double, align 8
  store double %load_acc31, ptr %alloca32, align 8
  store double 0x7FFB000000000000, ptr %ACC, align 8
  %load_acc33 = load double, ptr %ACC, align 8
  %alloca34 = alloca double, align 8
  store double %load_acc33, ptr %alloca34, align 8
  br label %block_exit35

block_exit35:                                     ; preds = %exit_block25
  %destroy_temp36 = load double, ptr %alloca32, align 8
  %destroy_temp37 = load double, ptr %alloca34, align 8
  br label %loop_station23

loop_body39:                                      ; preds = %loop_station40, %loop_exit24
  store double 1.000000e+00, ptr %ACC, align 8
  %load_acc42 = load double, ptr %ACC, align 8
  %alloca43 = alloca double, align 8
  store double %load_acc42, ptr %alloca43, align 8
  %load_variable44 = load double, ptr %alloca1, align 8
  store double %load_variable44, ptr %ACC, align 8
  %load_acc45 = load double, ptr %ACC, align 8
  %load_temp46 = load double, ptr %alloca43, align 8
  %sub = fsub double %load_temp46, %load_acc45
  store double %sub, ptr %ACC, align 8
  %load_acc47 = load double, ptr %ACC, align 8
  store double %load_acc47, ptr %alloca1, align 8
  %destroy_temp48 = load double, ptr %alloca43, align 8
  %load_variable49 = load double, ptr %alloca1, align 8
  store double %load_variable49, ptr %ACC, align 8
  %load_acc50 = load double, ptr %ACC, align 8
  %cast = bitcast double %load_acc50 to i64
  %and = and i64 %cast, 281474976710655
  %is_zero = icmp ne i64 %and, 0
  br i1 %is_zero, label %exit_block51, label %loop_exit41

loop_station40:                                   ; preds = %exit_block51
  br label %loop_body39

loop_exit41:                                      ; preds = %loop_body39
  store double 0x7FF9000000000001, ptr %ACC, align 8
  %load_acc52 = load double, ptr %ACC, align 8
  %cast53 = bitcast double %load_acc52 to i64
  %and54 = and i64 %cast53, 281474976710655
  %is_zero55 = icmp ne i64 %and54, 0
  br i1 %is_zero55, label %if, label %if_exit

exit_block51:                                     ; preds = %loop_body39
  br label %loop_station40

if:                                               ; preds = %loop_exit41
  store double 0x7FFE000000000000, ptr %ACC, align 8
  %load_acc56 = load double, ptr %ACC, align 8
  %alloca57 = alloca double, align 8
  store double %load_acc56, ptr %alloca57, align 8
  store double 0x7FFB000000000000, ptr %ACC, align 8
  %load_acc58 = load double, ptr %ACC, align 8
  %alloca59 = alloca double, align 8
  store double %load_acc58, ptr %alloca59, align 8
  br label %block_exit60

if_exit:                                          ; preds = %block_exit60, %loop_exit41
  br label %loop_body64

block_exit60:                                     ; preds = %if
  %destroy_temp61 = load double, ptr %alloca57, align 8
  %destroy_temp62 = load double, ptr %alloca59, align 8
  br label %if_exit

block_exit63:                                     ; preds = %loop_exit66, %loop_body64
  br label %try_blcok

loop_body64:                                      ; preds = %loop_station65, %if_exit
  br label %block_exit63

loop_station65:                                   ; preds = %dummy_block
  br label %loop_body64

loop_exit66:                                      ; No predecessors!
  br label %block_exit63

dummy_block:                                      ; No predecessors!
  br label %loop_station65

try_blcok:                                        ; preds = %block_exit63
  call void @RT_enter_try()
  call void @RT_exit_try()
  br label %exit_block67

catch_block:                                      ; No predecessors!
  call void @RT_exit_try()
  %load_exception = load double, ptr %ERROR, align 8
  store double %load_exception, ptr %ACC, align 8
  %alloca68 = alloca double, align 8
  %load_acc69 = load double, ptr %ACC, align 8
  store double %load_acc69, ptr %alloca68, align 8
  %load_variable70 = load double, ptr %alloca68, align 8
  store double %load_variable70, ptr %ACC, align 8
  %load_acc71 = load double, ptr %ACC, align 8
  %alloca72 = alloca double, align 8
  store double %load_acc71, ptr %alloca72, align 8
  store double 0x7FFB000000000000, ptr %ACC, align 8
  %load_acc73 = load double, ptr %ACC, align 8
  %alloca74 = alloca double, align 8
  store double %load_acc73, ptr %alloca74, align 8
  br label %block_exit75

exit_block67:                                     ; preds = %block_exit75, %try_blcok
  store double 0x7FFB000000000000, ptr %ACC, align 8
  %load_acc78 = load double, ptr %ACC, align 8
  %alloca79 = alloca double, align 8
  store double %load_acc78, ptr %alloca79, align 8
  store double 0x7FFB000000000000, ptr %ACC, align 8
  %load_acc80 = load double, ptr %ACC, align 8
  %alloca81 = alloca double, align 8
  store double %load_acc80, ptr %alloca81, align 8
  %load_temp82 = load double, ptr %alloca79, align 8
  store double %load_temp82, ptr %ACC, align 8
  %load_acc83 = load double, ptr %ACC, align 8
  %load_temp84 = load double, ptr %alloca81, align 8
  %eqeqeq = fcmp oeq double %load_temp84, %load_acc83
  %select = select i1 %eqeqeq, double 0x7FF9000000000001, double 0x7FF9000000000000
  store double %select, ptr %ACC, align 8
  %load_acc87 = load double, ptr %ACC, align 8
  %cast88 = bitcast double %load_acc87 to i64
  %and89 = and i64 %cast88, 281474976710655
  %is_zero90 = icmp ne i64 %and89, 0
  br i1 %is_zero90, label %if85, label %if_else

block_exit75:                                     ; preds = %catch_block
  %destroy_temp76 = load double, ptr %alloca72, align 8
  %destroy_temp77 = load double, ptr %alloca74, align 8
  br label %exit_block67

if85:                                             ; preds = %exit_block67
  br label %if_exit86

if_else:                                          ; preds = %exit_block67
  store double bitcast (i64 or (i64 ptrtoint (ptr @global_string to i64), i64 9223090561878065152) to double), ptr %ACC, align 8
  %load_acc91 = load double, ptr %ACC, align 8
  %alloca92 = alloca double, align 8
  store double %load_acc91, ptr %alloca92, align 8
  store double 0x7FFB000000000000, ptr %ACC, align 8
  %load_acc93 = load double, ptr %ACC, align 8
  %alloca94 = alloca double, align 8
  store double %load_acc93, ptr %alloca94, align 8
  br label %block_exit95

if_exit86:                                        ; preds = %block_exit95, %if85
  %destroy_temp98 = load double, ptr %alloca81, align 8
  %destroy_temp99 = load double, ptr %alloca79, align 8
  store double 0x7FFB000000000000, ptr %ACC, align 8
  br label %return_blcok

block_exit95:                                     ; preds = %if_else
  %destroy_temp96 = load double, ptr %alloca92, align 8
  %destroy_temp97 = load double, ptr %alloca94, align 8
  br label %if_exit86
}

define void @main() {
entry:
  %rt_entry = call double @RT_entry()
  %main = call double (double, ptr, i32, double, double, double, ...) @ddb7614e-f59b-48a6-83a0-f1acb20accef(double %rt_entry, ptr null, i32 0, double 0x7FFB000000000000, double 0x7FFB000000000000, double 0x7FFB000000000000)
  call void @RT_exit()
  ret void
}
