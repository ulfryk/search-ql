import { List } from 'immutable';

import { ValueType } from '../../../common/model';
import { Expression } from '../expression';

export class FunctionExpression extends Expression {

  public static fromParseResult(returnType: ValueType) {
    return (name: string, args: Expression[]) =>
      new FunctionExpression(List(args), name, returnType);
  }

  constructor(
    public readonly value: List<Expression>,
    public readonly name: string,
    public readonly returnType: ValueType,
  ) { super(); }

  // tslint:disable-next-line:cyclomatic-complexity
  public equals(other: Expression): boolean {
    return this === other || (
      other instanceof FunctionExpression &&
      this.name === other.name &&
      this.returnType === other.returnType &&
      this.value.equals(other.value)
    );
  }

  public rebuild() {
    const newValue = this.value.map(arg => arg.rebuild()).toList();

    if (newValue.equals(this.value)) {
      return this;
    }

    return new FunctionExpression(newValue, this.name, this.returnType);
  }

  public toString() {
    return `${this.name}( ${this.value.join(' , ')} )`;
  }

}
