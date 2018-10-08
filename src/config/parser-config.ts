import { List, Map, Set } from 'immutable';
import { Maybe } from 'monet';

import { OperatorType, ValueType } from '../common/model';
import { builtInFunctions as allBuiltInFunctions, FunctionConfig } from './function';
import { allParsers, ParserName } from './parser-names';

export class ParserConfig {

  public static create(
    {
      parserNames, model, builtInFunctions, customFunctions,
      AND, OR, GT, LT, GTE, LTE, IS, IS_NOT, LIKE, NOT_LIKE, NOT,
      GROUP_START, GROUP_END, EXACT_MATCHER,
      FN_LEFT_PAREN, FN_RIGHT_PAREN, FN_ARG_SEPARATOR,
    }: Partial<ParserConfig>,
  ) {
    return new ParserConfig(parserNames, model, builtInFunctions, customFunctions,
      AND, OR, LIKE, NOT_LIKE, GT, LT, IS, GTE, LTE, IS_NOT, NOT, GROUP_START, GROUP_END,
      EXACT_MATCHER, FN_LEFT_PAREN, FN_RIGHT_PAREN, FN_ARG_SEPARATOR);
  }

  public readonly operatorMapping = Map<string, OperatorType>([
    ...this.AND.map(token => [token, OperatorType.And]),
    ...this.OR.map(token => [token, OperatorType.Or]),

    ...this.LIKE.map(token => [token, OperatorType.Like]),
    ...this.NOT_LIKE.map(token => [token, OperatorType.NotLike]),

    ...this.GT.map(token => [token, OperatorType.Gt]),
    ...this.GTE.map(token => [token, OperatorType.Gte]),
    ...this.LT.map(token => [token, OperatorType.Lt]),
    ...this.LTE.map(token => [token, OperatorType.Lte]),
    ...this.IS.map(token => [token, OperatorType.Is]),
    ...this.IS_NOT.map(token => [token, OperatorType.IsNot]),

    ...this.NOT.map(token => [token, OperatorType.Not]),
  ]);

  public readonly functions = Map<string, FunctionConfig>(List([
    ...this.builtInFunctions,
    ...this.customFunctions,
  ]).flatMap(fnConfig => fnConfig.names.map(name => [name, fnConfig])));

  constructor(
    public readonly parserNames: ParserName[] = allParsers,
    public readonly model: Map<string, ValueType> = Map(),
    // functions
    public readonly builtInFunctions: FunctionConfig[] = allBuiltInFunctions,
    public readonly customFunctions: FunctionConfig[] = [],
    // binary operators
    public readonly AND = ['AND', '&'],
    public readonly OR = ['OR', '|'],
    public readonly LIKE = ['LIKE', '~'],
    public readonly NOT_LIKE = ['NOT LIKE', '!~'],
    public readonly GT = ['GT', '>'],
    public readonly LT = ['LT', '<'],
    public readonly IS = ['IS', '='],
    public readonly GTE = ['GTE', '>='],
    public readonly LTE = ['LTE', '<='],
    public readonly IS_NOT = ['IS NOT', '!=', 'ISNT'],
    // unary operators
    public readonly NOT = ['NOT', '!'],
    // grouping (only one sign allowed at once)
    public readonly GROUP_START = '(',
    public readonly GROUP_END = ')',
    public readonly EXACT_MATCHER = '"',
    // function
    public readonly FN_LEFT_PAREN = '(',
    public readonly FN_RIGHT_PAREN = ')',
    public readonly FN_ARG_SEPARATOR = ',',
  ) {
    const invalidTokens = this.restricted
      .filter(token => token.length !== 1)
      .map((token, name) => `${name}: "${token}"`);

    if (!invalidTokens.isEmpty()) {
      throw Error(`Some syntax tokens that have to be chars are not: ${invalidTokens.join(', ')}`);
    }
  }

  public get restrictedSigns() {
    return new RegExp(`^[^${this.restrictedJoined}]+`);
  }

  public get word() {
    return /^[\w_\.]+/;
  }

  public get binaryOperators() {
    const { AND, LIKE, NOT_LIKE, OR, GT, LT, IS, GTE, LTE, IS_NOT } = this;

    return [...AND, ...OR, ...LIKE, ...NOT_LIKE, ...GTE, ...LTE, ...GT, ...LT, ...IS_NOT, ...IS];
  }

  public get unaryOperators() {
    const { NOT } = this;

    return [...NOT];
  }

  public getOperatorType(token: string) {
    return Maybe.fromNull(this.operatorMapping.get(token)).cata(() => {
      throw Error(`No operator type for token: "${token}"`);
    }, __ => __);
  }

  private get restrictedJoined() {
    return this.restrictedDerivedFromOperators
      .add('s')
      .concat(this.restricted)
      .toArray()
      .map(sign => `\\${sign}`).join('');
  }

  private get restricted() {
    return Map<keyof ParserConfig, string>({
      ['EXACT_MATCHER']: this.EXACT_MATCHER,
      ['GROUP_END']: this.GROUP_END,
      ['GROUP_START']: this.GROUP_START,
      ['FN_LEFT_PAREN']: this.FN_LEFT_PAREN,
      ['FN_RIGHT_PAREN']: this.FN_RIGHT_PAREN,
      ['FN_ARG_SEPARATOR']: this.FN_ARG_SEPARATOR,
    });
  }

  private get restrictedDerivedFromOperators() {
    return Set([...this.binaryOperators, ...this.unaryOperators])
      .flatMap<string, string>(token => Array.from(token))
      .filter(sign => /[^\w\-_\s]/.test(sign))
      .toSet();
  }

}
