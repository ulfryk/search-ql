import { List, Set } from 'immutable';

import { Expression } from '../../common/model';
import { TermExpression } from './term';

export class InvalidExpression extends Expression {

  public static fromError(original: Expression, error: string) {
    return InvalidExpression.fromErrors(original, [error]);
  }

  public static fromErrors(original: Expression, errors: string[]) {
    return new InvalidExpression(original, Set(errors));
  }

  public static empty(error: string) {
    return InvalidExpression.fromError(TermExpression.empty(), error);
  }

  constructor(
    public readonly value: Expression,
    // TEMPORARY, should be a class extending same base as ParseFailure
    public readonly errors: Set<string>,
  ) { super(); }

  public get returnType() {
    return this.value.returnType;
  }

  public equals(other: Expression): boolean {
    return this === other || (
      other instanceof InvalidExpression &&
      this.value.equals(other.value) &&
      this.errors.equals(other.errors)
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
    return `InvalidExpression: ${this.errors.join('; ')}. EXPRESSION[ ${this.value} ]`;
  }

  public toList() {
    return List([this]).concat(this.value.toList()).toList();
  }

}
