



class Objec{
    constructor(value?: any){
        if (value != null){
            switch (typeof value){
                case "string":
                    toast_object_set_primitive_string(this, value as string);
                    break;
                case "number":
                    toast_object_set_primitive_number(this, value as number);
                    break;
                case "bigint":
                    toast_object_set_primitive_bigint(this, value as bigint);
                    break;
                case "symbol":
                    toast_object_set_primitive_symbol(this, value as symbol);
                    break;
                case "boolean":
                    toast_object_set_primitive_boolean(this, value as boolean);
                    break;
                case "undefined":
                    break;
                case "object":
                case "function":
                    throw TypeError("wrapping non primitive in object")
            }
        }
    }

    static assign(target: {}, ) {
        
    }
}