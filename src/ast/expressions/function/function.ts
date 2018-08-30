import { List } from 'immutable';

import { Expression } from '../expression';

export class FunctionExpression extends Expression {

  constructor(
    public readonly value: List<Expression>,
    public readonly name: string,
  ) { super(); }

  public equals(other: Expression): boolean {
    return this === other || (
      other instanceof FunctionExpression &&
      this.name === other.name &&
      this.value.equals(other.value)
    );
  }

  public rebuild() {
    const newValue = this.value.map(arg => arg.rebuild()).toList();

    if (newValue.equals(this.value)) {
      return this;
    }

    return new FunctionExpression(newValue, this.name);
  }

  public toString() {
    return `${this.name}( ${this.value.join(' , ')} )`;
  }

}
