import { List } from 'immutable';

import { Expression, ValueType } from '../../../common/model';

export abstract class TermExpression<PV = any> extends Expression {

  public readonly returnType: ValueType = ValueType.Text;

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
