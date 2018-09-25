import { None } from 'monet';

import { Expression } from '../expression';
import { Failure } from './failure';

export class TypeFailure extends Failure {

  public static fromError(ast: Expression) {
    return (error: string) => new TypeFailure(error, ast);
  }

  constructor(
    public readonly error: string,
    public readonly ast: Expression,
  ) {
    super([], None() /* TODO: use info from parsimmon's `node` when available */);
  }

  public equals(other: Failure): boolean {
    return this === other || (
      other instanceof TypeFailure &&
      this.error === other.error &&
      this.ast.equals(other.ast)
    );
  }

  public toString() {
    return `TypeError: ${this.error}`;
  }

}
