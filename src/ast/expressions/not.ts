import { List } from 'immutable';
import { None, Some } from 'monet';

import { ValueType } from '../../common/model';
import { Expression } from './expression';
import { InvalidExpression } from './invalid';
import { TermExpression } from './term';

export class NotExpression extends Expression {

  public static of(value: Expression) {
    return new NotExpression(value);
  }

  public static fromParseResult(__: string, operand: Expression) {
    return NotExpression.of(operand);
  }

  public readonly returnType = ValueType.Boolean;

  constructor(
    public readonly value: Expression,
  ) {
    super();
  }

  public equals(other: Expression): boolean {
    return this === other || (
      other instanceof NotExpression &&
      this.value.equals(other.value));
  }

  public isValid() {
    return this.value.isValid();
  }

  public checkTypes() {
    return this.getError()
      .foldLeft(this.clone(this.value.checkTypes()))(InvalidExpression.fromError);
  }

  public reshape() {
    return this.clone(this.value.reshape());
  }

  public toString() {
    return `NOT ${this.value}`;
  }

  public toList() {
    return List([this]).concat(this.value.toList()).toList();
  }

  private clone(newValue: Expression): Expression {
    if (this.value.equals(newValue)) {
      return this;
    }

    return new NotExpression(newValue);
  }

  private getError() {
    if (
      this.value.is(TermExpression as any) ||
      this.value.returnType === ValueType.Boolean
    ) {
      return None();
    }

    return Some('Operand of NOT operation should be a BOOLEAN');
  }

}
