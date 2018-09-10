import { List } from 'immutable';
import { Some } from 'monet';

import { Expression, isBooleanType, isPhraseType, ValueType } from '../../common/model';
import { InvalidExpression } from './invalid';
import { PhraseExpression } from './term';

export class NotExpression extends Expression {

  public static of(value: Expression) {
    return new NotExpression(value);
  }

  public static fromParseResult(__: string, operand: Expression) {
    return NotExpression.of(operand);
  }

  constructor(
    public readonly value: Expression,
  ) {
    super();
  }

  public get returnType(): ValueType {
    return isPhraseType(this.value.returnType) ? ValueType.Phrase : ValueType.Boolean;
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
    return Some(this.value)
      .filter(value => !(isBooleanType(value.returnType) || value.is(PhraseExpression as any)))
      .map(({ returnType }) =>
        `Operand of NOT operation should be a BOOLEAN, but got ${returnType}`);
  }

}
