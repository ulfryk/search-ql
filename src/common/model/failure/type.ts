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
    super([], { column: 0, line: 0, offset: 0 });
  }

  public toString() {
    return `TypeError: ${this.error}`;
  }

}
