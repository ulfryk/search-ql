import { Map, Set } from 'immutable';
import { Maybe } from 'monet';

import { OperatorType } from '../common/model';

export class SyntaxConfig {

  public static create(
    {
      AND, LIKE, OR,
      NOT,
      GROUP_START, GROUP_END, EXACT_MATCHER,
      FN_LEFT_PAREN, FN_RIGHT_PAREN, FN_ARG_SEPARATOR,
    }: Partial<SyntaxConfig>,
  ) {
    return new SyntaxConfig(AND, LIKE, OR, NOT, GROUP_START, GROUP_END, EXACT_MATCHER,
      FN_LEFT_PAREN, FN_RIGHT_PAREN, FN_ARG_SEPARATOR);
  }

  public readonly operatorMapping = Map<string, OperatorType>([
    ...this.AND.map(token => [token, OperatorType.And]),
    ...this.LIKE.map(token => [token, OperatorType.Like]),
    ...this.NOT.map(token => [token, OperatorType.Not]),
    ...this.OR.map(token => [token, OperatorType.Or]),
  ]);

  constructor(
    // binary operators
    public readonly AND = ['AND', '&'],
    public readonly LIKE = ['LIKE', '~'],
    public readonly OR = ['OR', '|'],
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
    const { AND, LIKE, OR } = this;

    return [...AND, ...LIKE, ...OR];
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
    return Map<keyof SyntaxConfig, string>({
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
