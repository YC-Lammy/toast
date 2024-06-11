
export const undefined: undefined = __native_ts_undefined();
export const globalThis:any = __native_ts_global_this();
export const Infinity: number = __native_ts_infinity();
export const NaN: number = __native_ts_nan();

export function isFinite(value: number): boolean{
    if (isNaN(value) || value === Infinity || value === -Infinity ){
        return false
    } else{
        return true
    }
}

export function isNaN(value: number): boolean{
    return __native_ts_is_nan(value)
}

export function parseFloat(value: string): number{
    return __native_ts_parse_float(value)
}

export function parseInt(value: string): number{
    return __native_ts_parse_int(value)
}


export {decodeURI, decodeURIComponent, encodeURI, encodeURIComponent} from './uri.ts'

declare type PropertyKey = string | number | symbol;

interface PropertyDescriptor {
    configurable?: boolean;
    enumerable?: boolean;
    value?: any;
    writable?: boolean;
    get?(): any;
    set?(v: any): void;
}

interface PropertyDescriptorMap {
    [key: PropertyKey]: PropertyDescriptor;
}

interface Object {
    /** The initial value of Object.prototype.constructor is the standard built-in Object constructor. */
    // constructor: Function;

    /** Returns a string representation of an object. */
    toString(): string;

    /** Returns a date converted to a string using the current locale. */
    toLocaleString(): string;

    /** Returns the primitive value of the specified object. */
    valueOf(): Object;

    /**
     * Determines whether an object has a property with the specified name.
     * @param v A property name.
     */
    hasOwnProperty(v: PropertyKey): boolean;

    /**
     * Determines whether an object exists in another object's prototype chain.
     * @param v Another object whose prototype chain is to be checked.
     */
    isPrototypeOf(v: Object): boolean;

    /**
     * Determines whether a specified property is enumerable.
     * @param v A property name.
     */
    propertyIsEnumerable(v: PropertyKey): boolean;
}

class ObjectClass {
    /**
     * Returns the prototype of an object.
     * @param o The object that references the prototype.
     */
    // static getPrototypeOf(o: any): any{};

    /**
     * Gets the own property descriptor of the specified object.
     * An own property descriptor is one that is defined directly on the object and is not inherited from the object's prototype.
     * @param o Object that contains the property.
     * @param p Name of the property.
     */
    static getOwnPropertyDescriptor(o: object, p: PropertyKey): PropertyDescriptor | undefined{
        if (__native_ts_reflect_has_own_key(o, p)){
            return {
                configurable: false,
                enumerable: true,
                writable: true,
                value: __native_ts_reflect_get(o, p),
            }
        };

        return undefined
    };

    /**
     * Returns the names of the own properties of an object. The own properties of an object are those that are defined directly
     * on that object, and are not inherited from the object's prototype. The properties of an object include both fields (objects) and functions.
     * @param o Object that contains the own properties.
     */
    static getOwnPropertyNames(o: object): string[]{
        return __native_ts_reflect_own_keys(o)
    };

    /**
     * Creates an object that has the specified prototype or that has null prototype.
     * @param o Object to use as a prototype. May be null.
     * @param properties JavaScript object that contains one or more property descriptors.
     */
    // static create(o: object | null, properties?: PropertyDescriptorMap): any{};

    /**
     * Adds a property to an object, or modifies attributes of an existing property.
     * @param o Object on which to add or modify the property. This can be a native JavaScript object (that is, a user-defined object or a built in object) or a DOM object.
     * @param p The property name.
     * @param attributes Descriptor for the property. It can be for a data property or an accessor property.
     */
    // static defineProperty<T>(o: T, p: PropertyKey, attributes: PropertyDescriptor): T{};

    /**
     * Adds one or more properties to an object, and/or modifies attributes of existing properties.
     * @param o Object on which to add or modify the properties. This can be a native JavaScript object or a DOM object.
     * @param properties JavaScript object that contains one or more descriptor objects. Each descriptor object describes a data property or an accessor property.
     */
    //static defineProperties<T>(o: T, properties: PropertyDescriptorMap & ThisType<any>): T{};

    /**
     * Prevents the modification of attributes of existing properties, and prevents the addition of new properties.
     * @param o Object on which to lock the attributes.
     */
    static seal<T>(o: T): T{
        // do nothing
        return o
    };

    /**
     * Prevents the modification of existing property attributes and values, and prevents the addition of new properties.
     * @param f Object on which to lock the attributes.
     */
    static freeze<T>(f: T): T{
        // do nothing
        return f
    };

    /**
     * Prevents the addition of new properties to an object.
     * @param o Object to make non-extensible.
     */
    static preventExtensions<T>(o: T): T{
        // do nothing
        return o
    };

    /**
     * Returns true if existing property attributes cannot be modified in an object and new properties cannot be added to the object.
     * @param o Object to test.
     */
    static isSealed(o: any): boolean{
        return false
    };

    /**
     * Returns true if existing property attributes and values cannot be modified in an object, and new properties cannot be added to the object.
     * @param o Object to test.
     */
    static isFrozen(o: any): boolean{
        return false
    };

    /**
     * Returns a value that indicates whether new properties can be added to an object.
     * @param o Object to test.
     */
    static isExtensible(o: any): boolean{
        return false
    };

    /**
     * Returns the names of the enumerable string properties and methods of an object.
     * @param o Object that contains the properties and methods. This can be an object that you created or an existing Document Object Model (DOM) object.
     */
    static keys(o: object): string[]{
        return __native_ts_reflect_keys(o)
    };
}

export var Object: ObjectClass = new ObjectClass;

export interface Function{
    length: number,
    name: string,
    [Symbol.hasInstance](o: any): boolean;
    // apply<T, R>(this: (this: T) => R, thisArg: T): ()=> R;
    // bind()
    toString(): string;
}

export var Function: {} = {};

export class Boolean{
    #value: boolean;
    constructor(value: boolean){
        this.#value = value;
    }
    toString(): string{
        if (this.#value){
            return "true"
        }
        return "false"
    }
    valueOf(): boolean{
        return this.#value
    }
}


export interface Symbol{
    description?: string;

    [Symbol.toPrimitive](): symbol;

    toString(): string;

    valueOf(): symbol;
}

export function Symbol(description?:string): symbol{
    if (description === undefined){
        return __native_ts_symbol_create("")
    } else{
        return __native_ts_symbol_create(description as string)
    }
}

export module Symbol{
    export const hasInstance: symbol = __native_ts_symbol_hasInstance();
    export const toPrimitive: symbol = __native_ts_symbol_primitive();
};

export interface Number{
    /**
     * Returns a string representation of an object.
     * @param radix Specifies a radix for converting numeric values to strings. This value is only used for numbers.
     */
    toString(radix?: number): string;

    /**
     * Returns a string representing a number in fixed-point notation.
     * @param fractionDigits Number of digits after the decimal point. Must be in the range 0 - 20, inclusive.
     */
    toFixed(fractionDigits?: number): string;

    /**
     * Returns a string containing a number represented in exponential notation.
     * @param fractionDigits Number of digits after the decimal point. Must be in the range 0 - 20, inclusive.
     */
    toExponential(fractionDigits?: number): string;

    /**
     * Returns a string containing a number represented either in exponential or fixed-point notation with a specified number of digits.
     * @param precision Number of significant digits. Must be in the range 1 - 21, inclusive.
     */
    toPrecision(precision?: number): string;

    /** Returns the primitive value of the specified object. */
    valueOf(): number;
}