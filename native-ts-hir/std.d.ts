
declare function __native_ts_undefined(): undefined;
declare function __native_ts_global_this(): any;
declare function __native_ts_infinity(): number;
declare function __native_ts_nan(): number;
declare function __native_ts_is_nan(value: number): boolean;
declare function __native_ts_parse_float(value: string): number;
declare function __native_ts_parse_int(value: string): number;
declare function __native_ts_decode_uri(encodedURI: string): string;

declare function __native_ts_symbol_create(description: string): symbol;
declare function __native_ts_symbol_hasInstance(): symbol;
declare function __native_ts_symbol_primitive(): symbol;

declare function __native_ts_reflect_has_own_key(obj: object, key: string | number | symbol): boolean;
declare function __native_ts_reflect_has_key(obj: object, key: string | number | symbol): boolean;
declare function __native_ts_reflect_own_keys(obj: object): string[];
declare function __native_ts_reflect_keys(obj: object): string[];
declare function __native_ts_reflect_get(obj: object, key: string | number | symbol): any;