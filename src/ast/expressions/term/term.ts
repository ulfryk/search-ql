import { List } from 'immutable';

import { Expression, ValueType } from '../../../common/model';

export class TermExpression<PV = any> extends Expression {

  public static of(value: string): TermExpression {
    return new TermExpression(value, value);
  }

  public static empty() {
    return new TermExpression('', null);
  }

  public readonly returnType: ValueType = ValueType.Boolean;

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

  public isValid() {
    return true;
  }

  public checkTypes() {
    return this;
  }

  public reshape() {
    return this;
  }

  public toString() {
    return `"${this.value}"`;
  }

  public toList() {
    return List([this]);
  }

}
