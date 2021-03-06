import { Expression } from '../expression';
import { Failure } from './failure';

export class IntegrityFailure extends Failure {

  public static fromError(ast: Expression) {
    return (error: string) => new IntegrityFailure(error, ast);
  }

  constructor(
    public readonly error: string,
    public readonly ast: Expression,
  ) {
    super([]);
  }

  public equals(other: Failure): boolean {
    return this === other || (
      other instanceof IntegrityFailure &&
      this.error === other.error &&
      this.ast.equals(other.ast)
    );
  }

  public toString() {
    return `IntegrityError: ${this.error}`;
  }

}
