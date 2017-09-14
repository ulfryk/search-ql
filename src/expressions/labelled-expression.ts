import { Map } from 'immutable';

import { Expression } from './expression';

export class LabelledExpression extends Expression {

  public readonly label: string;

  constructor(
    label: string,
    public readonly value: Expression,
  ) {
    super();
    this.label = label.toLowerCase();
  }

  public equals(other: Expression): boolean {
    return this === other || (
      other instanceof LabelledExpression &&
      this.label === other.label &&
      this.value.equals(other.value));
  }

  public toString() {
    return `${this.label}: ${this.value.toString()}`;
  }

  public test(values: Map<string, string>): boolean {
    return this.value.test(values
      .filter((_value, key) => key.includes(this.label)));
  }

}
