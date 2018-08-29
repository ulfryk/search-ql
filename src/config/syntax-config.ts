import { Map } from 'immutable';
import { Maybe } from 'monet';

import { OperatorType } from '../common/model';

export class SyntaxConfig {

  public static create(
    {
      AND, LIKE, OR,
      NOT,
      GROUP_START, GROUP_END, EXACT_MATCHER,
    }: Partial<SyntaxConfig>,
  ) {
    return new SyntaxConfig(AND, LIKE, OR, NOT, GROUP_START, GROUP_END, EXACT_MATCHER);
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
  ) {}

  public get restricted() {
    return [this.EXACT_MATCHER, this.GROUP_END, this.GROUP_START, 's']
      .map(sign => `\\${sign}`)
      .join('');
  }

  public get restrictedSigns() {
    return new RegExp(`^[^${this.restricted}]+`);
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

}
