function testInferP<T, P extends PathString>(props: {
  value: T;
  path: AutoCompletePath<T, P>;
}): P {
  return props.path;
}

// test1 should be inferred to 'foo', but is string in TS 4.7
// works in TS 4.6
const test1 = testInferP({ value: { foo: 42 }, path: 'foo' });

// test2 should be inferred to 'foo.bar', but is string in TS 4.7
// works in TS 4.6
const test2 = testInferP({ value: { foo: { bar: 42 } }, path: 'foo.bar' });

// Replacing the definition of AccessPattern with any fixes the issue in TS 4.7
// type AccessPattern<Get = unknown, Set = never> = any;

/**
 * Type which represents a property access.
 * @typeParam Get - the type when getting the property (covariant)
 * @typeParam Set - the type required for setting the property (contravariant)
 * @remarks
 * See {@link https://dmitripavlutin.com/typescript-covariance-contravariance/}
 * for an explanation of covariance and contravariance.
 *
 * Using it as a constraint:
 *  - `'abcd'` is a subtype   of `string`
 *  - `string` is a supertype of `'abcd'`
 *
 * Therefore,
 *  - `AccessPattern<'abcd', 'abcd'>` extends        `AccessPattern<string, 'abcd'>`
 *  - `AccessPattern<'abcd', string>` extends        `AccessPattern<'abcd', 'abcd'>`
 *  - `AccessPattern<string, 'abcd'>` doesn't extend `AccessPattern<'abcd', 'abcd'>`
 *  - `AccessPattern<'abcd', 'abcd'>` doesn't extend `AccessPattern<'abcd', string>`
 * @internal
 */
type AccessPattern<Get = unknown, Set = never> = (_: Set) => Get;

// ---------------------------------------------------------------------------------
//                             Other Type Definitions
// ---------------------------------------------------------------------------------

/**
 * Type alias to `string` which describes a lodash-like path through an object.
 * E.g. `'foo.bar.0.baz'`
 */
type PathString = string;

/**
 * Type, which given a path, returns the parent path as a string
 * @typeParam PT - path represented as a tuple
 * @example
 * ```
 * SuggestParentPath<['foo', 'bar', 'baz']> = 'foo.bar'
 * SuggestParentPath<['foo', 'bar']> = 'foo'
 * SuggestParentPath<['foo']> = never
 * ```
 * @internal
 */
type SuggestParentPath<PT extends PathTuple> = JoinPathTuple<
  PT extends [...infer R, Key] ? R : []
>;

/**
 * Type to implement {@link SuggestChildPaths}.
 * @typeParam PT  - the current path as a {@link PathTuple}
 * @typeParam TPT - the type at that path
 * @typeParam C   - constraint
 * @internal
 */
type SuggestChildPathsImpl<
  PT extends PathTuple,
  TPT,
  C extends AccessPattern,
> = JoinPathTuple<
  [
    ...PT,
    Keys<TPT, C> | Keys<TPT, AccessPattern<Traversable | undefined | null>>,
  ]
>;

/**
 * Type, which given a type and a path into the type, returns all paths as
 * strings which can be used to index the type at that path.
 * Filters out paths whose value doesn't match the constraint type or
 * aren't traversable.
 * @typeParam T  - type which is indexed by the path
 * @typeParam PT - the current path into the type as a tuple
 * @typeParam C  - constraint
 * @example
 * ```
 * SuggestChildPaths<{foo: string, bar: string}, [], AccessPattern<string>>
 *   = 'foo' | 'bar'
 * SuggestChildPaths<{foo: string, bar: number}, [], AccessPattern<string>>
 *   = 'foo'
 * SuggestChildPaths<{foo: {bar: string}}, ['foo']> = 'foo.bar'
 * SuggestChildPaths<{foo: {bar: string[]}}, ['foo']> = 'foo.bar'
 * ```
 * @internal
 */
type SuggestChildPaths<
  T,
  PT extends PathTuple,
  C extends AccessPattern = AccessPattern,
> = PT extends any ? SuggestChildPathsImpl<PT, PathGetValue<T, PT>, C> : never;

/**
 * Type to implement {@link SuggestPaths} without having to compute the valid
 * path prefix more than once.
 * @typeParam T   - type which is indexed by the path
 * @typeParam PT  - the current path into the type as a tuple
 * @typeParam C   - constraint
 * @typeParam VPT - the valid path prefix for the given path
 * @internal
 */
type SuggestPathsImpl<
  T,
  PT extends PathTuple,
  C extends AccessPattern,
  VPT extends PathTuple,
> =
  | SuggestChildPaths<T, VPT, C>
  | (PT extends VPT ? SuggestParentPath<VPT> : JoinPathTuple<VPT>);

/**
 * Type which given a type and a path tuple into it returns
 *  - its parent/predecessor path string.
 *  - all its child/successor paths that point to a type which is either
 *    traversable or matches the constraint type.
 * In case the path does not exist it returns all of the above, but for the last
 * valid path prefix.
 * @typeParam T  - type which is indexed by the path
 * @typeParam PT - the current path into the type as a tuple
 * @typeParam C  - constraint
 * @example
 * ```
 * SuggestPaths<{foo: {bar: string}}, ['foo'], string> = 'foo.bar'
 * SuggestPaths<{foo: {bar: string}}, ['foo', 'ba'], AccessPattern<string>>
 *   = 'foo' | 'foo.bar'
 * SuggestPaths<{foo: {bar: string}}, ['foo', 'bar'], AccessPattern<string>>
 *   = 'foo'
 * SuggestPaths<{foo: {bar: {baz: string}}}, ['foo', 'bar'], AccessPattern<string>>
 *   = 'foo' | 'foo.bar.baz'
 * ```
 * @internal
 */
type SuggestPaths<
  T,
  PT extends PathTuple,
  C extends AccessPattern = AccessPattern,
> = SuggestPathsImpl<T, PT, C, ValidPathPrefix<T, PT>>;

/**
 * Type to test whether the path is a union of paths.
 * @typeParam PS - path
 * @example
 * ```
 * IsPathUnion<'foo'> = false
 * IsPathUnion<'foo' | 'foo'> = false
 * IsPathUnion<'foo' | 'foo.bar'> = true
 * ```
 * @internal
 */
type IsPathUnion<PS extends PathString> = IsNever<UnionToIntersection<PS>>;

/**
 * Type to check the current path against the constraint type.
 * Returns the path if it is valid and matches the constraint type.
 * @typeParam T  - type which is indexed by the path
 * @typeParam PS - the current path into the type as a string,
 *                 mustn't be a union
 * @typeParam PT - the current path into the type as a tuple,
 *                 must be equal to SplitPathString<PS>
 * @typeParam C  - constraint
 * @example
 * ```
 * AutoCompletePathCheckConstraint<{foo: {bar: string}}, 'foo', ['foo'], AccessPattern<string>>
 *   = never
 * AutoCompletePathCheckConstraint<{foo: {bar: string}}, 'foo.ba', ['foo', 'ba'], AccessPattern<string>>
 *   = never
 * AutoCompletePathCheckConstraint<{foo: {bar: string}}, 'foo.bar', ['foo', 'bar'], AccessPattern<string>>
 *   = 'foo.bar'
 * ```
 * @internal
 */
type AutoCompletePathCheckConstraint<
  T,
  PS extends PathString,
  PT extends PathTuple,
  C extends AccessPattern,
> = HasPath<T, PT> extends true
  ? AccessPattern<PathGetValue<T, PT>, PathSetValue<T, PT>> extends C
    ? PS extends JoinPathTuple<PT>
      ? PS
      : JoinPathTuple<PT>
    : never
  : never;

/**
 * Type to implement {@link AutoCompletePath} without having to compute the
 * path tuple more than once.
 * @typeParam T  - type which is indexed by the path
 * @typeParam PS - the current path into the type as a string,
 *                 mustn't be a union
 * @typeParam PT - the current path into the type as a tuple,
 *                 must be equal to SplitPathString<PS>
 * @typeParam C  - constraint
 * @example
 * ```
 * AutoCompletePathImpl<{foo: {bar: string}}, 'foo', ['foo'], AccessPattern<string>>
 *   = 'foo.bar'
 * AutoCompletePathImpl<{foo: {bar: string}}, 'foo.ba', ['foo', 'ba'], AccessPattern<string>>
 *   = 'foo' | 'foo.bar'
 * AutoCompletePathImpl<{foo: {bar: string}}, 'foo.bar', ['foo', 'bar'], AccessPattern<string>>
 *   = 'foo' | 'foo.bar'
 * AutoCompletePathImpl<{foo: {bar: {baz: string}}}, 'foo.bar', ['foo', 'bar'], AccessPattern<string>>
 *   = 'foo' | 'foo.bar.baz'
 * ```
 * @internal
 */
type AutoCompletePathImpl<
  T,
  PS extends PathString,
  PT extends PathTuple,
  C extends AccessPattern,
> = SuggestPaths<T, PT, C> | AutoCompletePathCheckConstraint<T, PS, PT, C>;

/**
 * Type which given a type and a path string into it returns
 *  - its parent/predecessor path string.
 *  - the path string itself, if it exists within the type and matches
 *    the constraint type.
 *  - all its child/successor paths that point to a type which is either
 *    traversable or matches the constraint type.
 * Also,
 *  - in case the path does not exist it returns all of the above for the last
 *    valid path.
 *  - in case the path is a union of paths it doesn't suggest any
 *    parent/predecessor and child/successor paths.
 *    Otherwise, the returned type may become to large, or it may accept paths
 *    which don't match the constraint type.
 * @typeParam T  - type which is indexed by the path
 * @typeParam PS - the current path into the type as a string
 * @typeParam C  - constraint
 * @example
 * ```
 * AutoCompletePath<{foo: {bar: string}}, 'foo', AccessPattern<string>>
 *   = 'foo.bar'
 * AutoCompletePath<{foo: {bar: string}}, 'foo.ba', AccessPattern<string>>
 *   = 'foo' | 'foo.bar'
 * AutoCompletePath<{foo: {bar: string}}, 'foo.bar', AccessPattern<string>>
 *   = 'foo' | 'foo.bar'
 * AutoCompletePath<{foo: {bar: {baz: string}}}, 'foo.bar', AccessPattern<string>>
 *   = 'foo' | 'foo.bar.baz'
 * ```
 * @internal
 */
type AutoCompletePath<
  T,
  PS extends PathString,
  C extends AccessPattern = AccessPattern,
> = IsAny<PS> extends true
  ? any
  : IsPathUnion<PS> extends false
  ? AutoCompletePathImpl<T, PS, SplitPathString<PS>, C>
  : PS extends any
  ? AutoCompletePathCheckConstraint<T, PS, SplitPathString<PS>, C>
  : never;

/**
 * Type to check whether a key is present in a type.
 * If a union of keys is passed, all keys have to be present in the type.
 * @typeParam T - type which is introspected
 * @typeParam K - key
 * @example
 * ```
 * HasKey<{foo: string}, 'foo'> = true
 * HasKey<{foo: string}, 'bar'> = false
 * HasKey<{foo: string}, 'foo' | 'bar'> = false
 * ```
 * @internal
 */
type HasKey<T, K extends Key> = IsNever<Exclude<K, Keys<T>>>;

/**
 * Type to implement {@link ValidPathPrefix} tail recursively.
 * @typeParam T   - type which the path should be checked against
 * @typeParam PT  - path which should exist within the given type
 * @typeParam VPT - accumulates the prefix of keys which have been
 *                  confirmed to exist already
 * @internal
 */
type ValidPathPrefixImpl<
  T,
  PT extends PathTuple,
  VPT extends PathTuple,
> = PT extends [infer K, ...infer R]
  ? HasKey<T, AsKey<K>> extends true
    ? ValidPathPrefixImpl<
        KeyGetValue<T, AsKey<K>>,
        AsPathTuple<R>,
        AsPathTuple<[...VPT, K]>
      >
    : VPT
  : VPT;

/**
 * Type to find the longest path prefix which is still valid,
 * i.e. exists within the given type.
 * @typeParam T  - type which the path should be checked against
 * @typeParam PT - path which should exist within the given type
 * @example
 * ```
 * ValidPathPrefix<{foo: {bar: string}}, ['foo', 'bar']> = ['foo', 'bar']
 * ValidPathPrefix<{foo: {bar: string}}, ['foo', 'ba']> = ['foo']
 * ```
 * @internal
 */
type ValidPathPrefix<T, PT extends PathTuple> = ValidPathPrefixImpl<T, PT, []>;

/**
 * Type to check whether a path through a type exists.
 * @typeParam T  - type which the path should be checked against
 * @typeParam PT - path which should exist within the given type
 * @example
 * ```
 * HasPath<{foo: {bar: string}}, ['foo', 'bar']> = true
 * HasPath<{foo: {bar: string}}, ['foo', 'ba']> = false
 * ```
 * @internal
 */
type HasPath<T, PT extends PathTuple> = ValidPathPrefix<T, PT> extends PT
  ? true
  : false;

/**
 * Type which given a tuple type returns its own keys, i.e. only its indices.
 * @typeParam T - tuple type
 * @example
 * ```
 * TupleKeys<[number, string]> = '0' | '1'
 * ```
 * @internal
 */
type TupleKeys<T extends ReadonlyArray<any>> = Exclude<keyof T, keyof any[]>;

/**
 * Type which extracts all numeric keys from an object.
 * @typeParam T - type
 * @example
 * ```
 * NumericObjectKeys<{0: string, '1': string, foo: string}> = '0' | '1'
 * ```
 * @internal
 */
type NumericObjectKeys<T extends Traversable> = ToKey<
  Extract<keyof T, ArrayKey | `${ArrayKey}`>
>;

/**
 * Type which extracts all numeric keys from an object, tuple, or array.
 * If a union is passed, it evaluates to the overlapping numeric keys.
 * @typeParam T - type
 * @example
 * ```
 * NumericKeys<{0: string, '1': string, foo: string}> = '0' | '1'
 * NumericKeys<number[]> = `${number}`
 * NumericKeys<[string, number]> = '0' | '1'
 * NumericKeys<{0: string, '1': string} | [number] | number[]> = '0'
 * ```
 * @internal
 */
type NumericKeys<T extends Traversable> = UnionToIntersection<
  T extends ReadonlyArray<any>
    ? IsTuple<T> extends true
      ? [TupleKeys<T>]
      : [ToKey<ArrayKey>]
    : [NumericObjectKeys<T>]
>[never];

/**
 * Type which extracts all keys from an object.
 * If a union is passed, it evaluates to the overlapping keys.
 * @typeParam T - object type
 * @example
 * ```
 * ObjectKeys<{foo: string, bar: string}, string> = 'foo' | 'bar'
 * ObjectKeys<{foo: string, bar: number}, string> = 'foo'
 * ```
 * @internal
 */
type ObjectKeys<T extends Traversable> = Exclude<
  ToKey<keyof T>,
  `${string}.${string}` | ''
>;

/**
 * Type to check whether a type's property matches the constraint type
 * and return its key. Converts the key to a string.
 * @typeParam T - type whose property should be checked
 * @typeParam K - key of the property
 * @typeParam C - constraint
 * @example
 * ```
 * CheckKeyConstraint<{foo: string}, 'foo', AccessPattern<string>> = 'foo'
 * CheckKeyConstraint<{foo: string}, 'foo', AccessPattern<number>> = never
 * CheckKeyConstraint<string[], number, AccessPattern<string>> = `${number}`
 * ```
 * @internal
 */
type CheckKeyConstraint<
  T,
  K extends Key,
  C extends AccessPattern,
> = K extends any
  ? AccessPattern<KeyGetValue<T, K>, KeySetValue<T, K>> extends C
    ? K
    : never
  : never;

/**
 * Type which evaluates to true when the type is an array or tuple or is a union
 * which contains an array or tuple.
 * @typeParam T - type
 * @example
 * ```
 * ContainsIndexable<{foo: string}> = false
 * ContainsIndexable<{foo: string} | number[]> = true
 * ```
 * @internal
 */
type ContainsIndexable<T> = IsNever<Extract<T, ReadonlyArray<any>>> extends true
  ? false
  : true;

/**
 * Type to implement {@link Keys} for non-nullable values.
 * @typeParam T - non-nullable type whose property should be checked
 * @internal
 */
type KeysImpl<T> = [T] extends [Traversable]
  ? ContainsIndexable<T> extends true
    ? NumericKeys<T>
    : ObjectKeys<T>
  : never;

/**
 * Type to find all properties of a type that match the constraint type
 * and return their keys.
 * If a union is passed, it evaluates to the overlapping keys.
 * @typeParam T - type whose property should be checked
 * @typeParam C - constraint
 * @example
 * ```
 * Keys<{foo: string, bar: string}, AccessPattern<string>> = 'foo' | 'bar'
 * Keys<{foo?: string, bar?: string}> = 'foo' | 'bar'
 * Keys<{foo: string, bar: number}, AccessPattern<string>> = 'foo'
 * Keys<[string, number], string> = '0'
 * Keys<string[], AccessPattern<string>> = `${number}`
 * Keys<{0: string, '1': string} | [number] | number[]> = '0'
 * ```
 * @internal
 */
type Keys<T, C extends AccessPattern = AccessPattern> = IsAny<T> extends true
  ? Key
  : IsNever<T> extends true
  ? Key
  : IsNever<NonNullable<T>> extends true
  ? never
  : CheckKeyConstraint<T, KeysImpl<NonNullable<T>>, C>;

/**
 * Type to access a type by a key.
 *  - Returns undefined if it can't be indexed by that key.
 *  - Returns null if the type is null.
 *  - Returns undefined if the type is not traversable.
 * @typeParam T - type which is indexed by the key
 * @typeParam K - key into the type
 * ```
 * TryGet<{foo: string}, 'foo'> = string
 * TryGet<{foo: string}, 'bar'> = undefined
 * TryGet<null, 'foo'> = null
 * TryGet<string, 'foo'> = undefined
 * ```
 * @internal
 */
type TryGet<T, K> = K extends keyof T
  ? T[K]
  : T extends null | undefined
  ? T
  : unknown;

/**
 * Type to access an array type by a key.
 * @typeParam T - type which is indexed by the key
 * @typeParam K - key into the type
 * ```
 * TryGetArray<string[], '0'> = string
 * TryGetArray<string[], 'foo'> = undefined
 * ```
 * @internal
 */
type TryGetArray<
  T extends ReadonlyArray<any>,
  K extends Key,
> = K extends `${ArrayKey}` ? T[number] : TryGet<T, K>;

/**
 * Type to evaluate the type which the given key points to.
 *  - If either T or K is union, it will evaluate to the union of the types at
 *    the given key(s).
 *  - If T can be null or undefined, the resulting type will also include null
 *    or undefined.
 *  - If a key doesn't exist, or may be optional, the resulting type will
 *    include undefined.
 * @typeParam T - type which is indexed by the key
 * @typeParam K - key into the type
 * @example
 * ```
 * KeyGetValue<{foo: string}, 'foo'> = string
 * KeyGetValue<{foo: string, bar: number}, 'foo' | 'bar'> = string | number
 * KeyGetValue<{foo: string} | {foo: number}, 'foo'> = string | number
 * KeyGetValue<null | {foo: string}, 'foo'> = null | string
 * KeyGetValue<{bar: string}, 'foo'> = undefined
 * KeyGetValue<{foo?: string}, 'foo'> = undefined | string
 * ```
 * @internal
 */
type KeyGetValue<T, K extends Key> = IsNever<K> extends true
  ? unknown
  : T extends ReadonlyArray<any>
  ? IsTuple<T> extends true
    ? TryGet<T, K>
    : TryGetArray<T, K>
  : TryGet<MapKeys<T>, K>;

/**
 * Type to evaluate the type which the given path points to.
 *  - If either T or PT is union, it will evaluate to the union of the types at
 *    the given path(s).
 *  - If T can be null or undefined, the resulting type will also include null
 *    or undefined.
 *  - If a path doesn't exist, or may be optional, the resulting type will
 *    include undefined.
 * @typeParam T  - deeply nested type which is indexed by the path
 * @typeParam PT - path into the deeply nested type
 * @example
 * ```
 * PathGetValue<{foo: {bar: string}}, ['foo', 'bar']> = string
 * PathGetValue<{foo: string, bar: number}, ['foo'] | ['bar']> = string | number
 * PathGetValue<{foo: string} | {foo: number}, ['foo']> = string | number
 * PathGetValue<null | {foo: string}, ['foo']> = null | string
 * PathGetValue<{bar: string}, ['foo']> = undefined
 * PathGetValue<{foo?: string}, ['foo']> = undefined | string
 * ```
 * @internal
 */
type PathGetValue<T, PT extends PathTuple> = IsAny<PT> extends true
  ? any
  : IsNever<PT> extends true
  ? unknown
  : PT extends [infer K, ...infer R]
  ? PathGetValue<KeyGetValue<T, AsKey<K>>, AsPathTuple<R>>
  : T;

/**
 * Type to access a type by a key. Returns never
 *  - if it can't be indexed by that key.
 *  - if the type is not traversable.
 * @typeParam T - type which is indexed by the key
 * @typeParam K - key into the type
 * ```
 * TrySet<{foo: string}, 'foo'> = string
 * TrySet<{foo: string}, 'bar'> = never
 * TrySet<null, 'foo'> = never
 * TrySet<string, 'foo'> = never
 * ```
 * @internal
 */
type TrySet<T, K> = K extends keyof T ? T[K] : never;

/**
 * Type to access an array type by a key.
 * @typeParam T - type which is indexed by the key
 * @typeParam K - key into the type
 * ```
 * TrySetArray<string[], '0'> = string
 * TrySetArray<string[], 'foo'> = never
 * ```
 * @internal
 */
type TrySetArray<
  T extends ReadonlyArray<any>,
  K extends Key,
> = K extends `${ArrayKey}` ? T[number] : TrySet<T, K>;

/**
 * Type to implement {@link KeySetValue}. Wraps everything into a tuple.
 * @typeParam T - non-nullable type which is indexed by the key
 * @typeParam K - key into the type, mustn't be a union of keys
 * @internal
 */
type KeySetValueImpl<T, K extends Key> = T extends ReadonlyArray<any>
  ? IsTuple<T> extends true
    ? [TrySet<T, K>]
    : [TrySetArray<T, K>]
  : [TrySet<MapKeys<T>, K>];

/**
 * Type to evaluate the type which is required for setting the property which
 * the given key points to.
 *  - If either T or K is union, it will evaluate to the intersection of the
 *    types at the given key(s).
 *  - If T can be null or undefined, the resulting type won't include null or
 *    undefined.
 *  - If a key doesn't exist,the resulting type will be never.
 *  - If a key may be optional, the resulting type will include undefined.
 * @typeParam T - type which is indexed by the key
 * @typeParam K - key into the type
 * @example
 * ```
 * KeySetValue<{foo: string}, 'foo'> = string
 * KeySetValue<{foo: string, bar: number}, 'foo' | 'bar'> = string & number
 * KeySetValue<{foo: string} | {foo: number}, 'foo'> = string & number
 * KeySetValue<null | {foo: string}, 'foo'> = string
 * KeySetValue<{bar: string}, 'foo'> = never
 * KeySetValue<{foo?: string}, 'foo'> = undefined | string
 * ```
 * @internal
 */
type KeySetValue<T, K extends Key> = UnionToIntersection<
  K extends any ? KeySetValueImpl<NonNullable<T>, K> : never
>[never];

/**
 * Type to implement {@link PathSetValue} tail-recursively.
 * Wraps everything into a tuple.
 * @typeParam T  - deeply nested type which is indexed by the path
 * @typeParam PT - path into the deeply nested type
 * @internal
 */
type PathSetValueImpl<T, PT extends PathTuple> = PT extends [
  infer K,
  ...infer R,
]
  ? PathSetValueImpl<KeySetValue<T, AsKey<K>>, AsPathTuple<R>>
  : [T];

/**
 * Type to evaluate the type which is required for setting the property which
 * the given path points to.
 *  - If either T or PT is union, it will evaluate to the intersection of the
 *    types at the given paths(s).
 *  - If T can be null or undefined, the resulting type won't include null or
 *    undefined.
 *  - If a path doesn't exist, the resulting type will be never.
 *  - Only if last kay is optional, the resulting type will include undefined.
 * @typeParam T  - deeply nested type which is indexed by the path
 * @typeParam PT - path into the deeply nested type
 * @example
 * ```
 * PathSetValue<{foo: {bar: string}}, ['foo', 'bar']> = string
 * PathSetValue<{foo: string, bar: number}, ['foo'] | ['bar']> = string & number
 * PathSetValue<{foo: string} | {foo: number}, ['foo']> = string & number
 * PathSetValue<null | {foo: string}, ['foo']> = string
 * PathSetValue<{bar: string}, ['foo']> = never
 * PathSetValue<{foo?: string}, ['foo']> = undefined | string
 * PathSetValue<{foo?: {bar: string}}, ['foo', 'bar']> = string
 * ```
 * @internal
 */
type PathSetValue<T, PT extends PathTuple> = IsAny<PT> extends true
  ? any
  : UnionToIntersection<PathSetValueImpl<T, PT>>[never];

/**
 * Type which describes a path through an object as a list of individual keys.
 * @internal
 */
type PathTuple = Key[];

/**
 * Type to assert that a type is a path tuple.
 * @typeParam T - type which may be a path tuple
 * @internal
 */
type AsPathTuple<T> = Extract<T, PathTuple>;

/**
 * Type which appends a key to the path tuple only if it is not blank,
 * i.e. not the empty string.
 * @typeParam PT - path
 * @typeParam K  - key
 * @example
 * ```
 * AppendNonBlankKey<['foo'], 'bar'> = ['foo', 'bar']
 * AppendNonBlankKey<['foo'], ''> = ['foo']
 * ```
 * @internal
 */
type AppendNonBlankKey<PT extends PathTuple, K extends Key> = K extends ''
  ? PT
  : [...PT, K];

/**
 * Type to implement {@link SplitPathString} tail recursively.
 * @typeParam PS - remaining path string which should be split into its
 *                 individual keys
 * @typeParam PT - accumulator of the keys which have been split from
 *                 the original path string already
 * @internal
 */
type SplitPathStringImpl<
  PS extends PathString,
  PT extends PathTuple,
> = PS extends `${infer K}.${infer R}`
  ? SplitPathStringImpl<R, AppendNonBlankKey<PT, K>>
  : AppendNonBlankKey<PT, PS>;

/**
 * Type to split a path string into a path tuple.
 * The individual keys may be empty strings.
 * @typeParam PS  - path string which should be split into its individual keys
 * @example
 * ```
 * SplitPathString<'foo'> = ['foo']
 * SplitPathString<'foo.bar.0.baz'> = ['foo', 'bar', '0', 'baz']
 * SplitPathString<'.'> = []
 * ```
 * @internal
 */
type SplitPathString<PS extends PathString> = IsAny<PS> extends true
  ? any
  : SplitPathStringImpl<PS, []>;

/**
 * Type to implement {@link JoinPathTuple} tail-recursively.
 * @typeParam PT - remaining keys which needs to be joined
 * @typeParam PS - accumulator of the already joined keys
 * @internal
 */
type JoinPathTupleImpl<
  PT extends PathTuple,
  PS extends PathString,
> = PT extends [infer K, ...infer R]
  ? JoinPathTupleImpl<AsPathTuple<R>, `${PS}.${AsKey<K>}`>
  : PS;

/**
 * Type to join a path tuple to a path string.
 * @typeParam PT - path tuple which should be joined.
 * @example
 * ```
 * JoinPathTuple<['foo']> = 'foo'
 * JoinPathTuple<['foo', 'bar', '0', 'baz']> = 'foo.bar.0.baz'
 * JoinPathTuple<[]> = never
 * ```
 * @internal
 */
type JoinPathTuple<PT extends PathTuple> = PT extends [infer K, ...infer R]
  ? JoinPathTupleImpl<AsPathTuple<R>, AsKey<K>>
  : never;

/**
 * Type which can be traversed through with a path string.
 * I.e. objects, arrays, and tuples
 * @internal
 */
type Traversable = object;

/**
 * Type which can be used to index an array or tuple type.
 * @internal
 */
type ArrayKey = number;

/**
 * Type which can be used to index an object.
 * @internal
 */
type Key = string;

/**
 * Type to assert that a type is a key.
 * @typeParam T - type which may be a key
 * @internal
 */
type AsKey<T> = Extract<T, Key>;

/**
 * Type to convert a type to a key.
 * @typeParam T - type which may be converted to a key
 * @internal
 */
type ToKey<T> = T extends ArrayKey ? `${T}` : AsKey<T>;

/**
 * Type which converts all keys of an object to keys.
 * @typeParam T - object type
 * @example
 * ```
 * MapKeys<{0: string}> = {'0': string}
 * ```
 * @internal
 */
type MapKeys<T> = { [K in keyof T as ToKey<K>]: T[K] };

/**
 * Type to query whether an array type T is a tuple type.
 * @typeParam T - type which may be an array or tuple
 * @example
 * ```
 * IsTuple<[number]> = true
 * IsTuple<number[]> = false
 * ```
 * @internal
 */
type IsTuple<T extends ReadonlyArray<any>> = number extends T['length']
  ? false
  : true;

/**
 * Type to intersect a union type.
 * See https://fettblog.eu/typescript-union-to-intersection/
 * @typeParam U - union
 * @example
 * ```
 * UnionToIntersection<{ foo: string } | { bar: number }>
 *   = { foo: string; bar: number }
 * ```
 * @internal
 */
type UnionToIntersection<U> = (U extends any ? (_: U) => any : never) extends (
  _: infer I,
) => any
  ? I
  : never;

/**
 * Checks whether the type is any
 * See {@link https://stackoverflow.com/a/49928360/3406963}
 * @typeParam T - type which may be any
 * ```
 * IsAny<any> = true
 * IsAny<string> = false
 * ```
 */
type IsAny<T> = 0 extends 1 & T ? true : false;

/**
 * Checks whether the type is never
 * @typeParam T - type which may be never
 * ```
 * IsNever<never> = true
 * IsNever<string> = false
 * ```
 */
type IsNever<T> = [T] extends [never] ? true : false;
