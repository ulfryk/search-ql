import { Expression } from '../expression';

export class TermExpression<PV = any> extends Expression {

  public static fromMatch(_match: string): TermExpression {
    throw Error('unimplemented');
  }

  public static empty() {
    return new TermExpression('', null);
  }

  constructor(
    public readonly value: string,
    public readonly preparedValue: PV,
  ) {
    super();
  }

  public equals(other: Expression): boolean {
    return this === other || (
      other instanceof this.constructor &&
      this.preparedValue === (other as TermExpression).preparedValue
    );
  }

  public rebuild() {
    return this;
  }

  public toString() {
    return this.value;
  }

}
