import { Failure } from './failure';

export class TypeFailure extends Failure {

  constructor(
    public readonly error: string,
  ) {
    super([], { column: 0, line: 0, offset: 0 });
  }

  public toString() {
    return `TypeError: ${this.error}`;
  }

}
