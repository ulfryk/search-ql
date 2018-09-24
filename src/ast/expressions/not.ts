import { List, Map } from 'immutable';
import { Maybe, None, Some } from 'monet';

import { Expression, ExpressionType, isBooleanType, isPhraseType, ValueType } from '../../common/model';
import { INotExpression } from '../../dto';
import { InvalidExpression } from './invalid';
import { PhraseExpression, TermExpression } from './term';

export class NotExpression extends Expression {

  public static of(value: Expression) {
    return new NotExpression(value);
  }

  public static fromParseResult(__: string, operand: Expression) {
    return NotExpression.of(operand);
  }

  public readonly type: ExpressionType.Not = ExpressionType.Not;

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
    return this.getTypeError()
      .foldLeft(this.clone(this.value.checkTypes()))(InvalidExpression.fromError);
  }

  public checkIntegrity(model: Map<string, ValueType>) {
    return this.getIntegrityError()
      .foldLeft(this.clone(this.value.checkIntegrity(model)))(InvalidExpression.fromError);
  }

  public reshape() {
    const value = this.value.is(TermExpression as any) ?
      PhraseExpression.fromTerm(this.value as TermExpression) : this.value;

    return this.clone(value.reshape());
  }

  public toString() {
    return `NOT ${this.value}`;
  }

  public toList() {
    return List([this]).concat(this.value.toList()).toList();
  }

  public toJS(): INotExpression {
    return {
      returnType: this.returnType,
      type: this.type,
      value: this.value.toJS(),
    };
  }

  private clone(newValue: Expression): Expression {
    if (this.value.equals(newValue)) {
      return this;
    }

    return new NotExpression(newValue);
  }

  private getTypeError() {
    return Some(this.value)
      .filter(value => !(isBooleanType(value.returnType) || value.is(PhraseExpression as any)))
      .map(({ returnType }) =>
        `Operand of NOT operation should be a BOOLEAN, but got ${returnType}`);
  }

  private getIntegrityError(): Maybe<string> {
    // TODO: Integrity
    return None();
  }

}
