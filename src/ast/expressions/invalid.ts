import { List } from 'immutable';

import { Expression } from './expression';
import { TermExpression } from './term';

export class InvalidExpression extends Expression {

  public static fromError(original: Expression, error: string) {
    return new InvalidExpression(original, error);
  }

  public static empty(error: string) {
    return new InvalidExpression(TermExpression.empty(), error);
  }

  constructor(
    public readonly value: Expression,
    // TEMPORARY, should be a class extending same base as ParseFailure
    public readonly error: string,
  ) { super(); }

  public get returnType() {
    return this.value.returnType;
  }

  public equals(other: Expression): boolean {
    return this === other || (
      other instanceof InvalidExpression &&
      this.value.equals(other.value) &&
      this.error === other.error
    );
  }

  public isValid() {
    return false;
  }

  public checkTypes() {
    return this;
  }

  public reshape() {
    return this;
  }

  public toString() {
    return `InvalidExpression: ${this.error}. EXPRESSION[ ${this.value} ]`;
  }

  public toList() {
    return List([this]);
  }

}
