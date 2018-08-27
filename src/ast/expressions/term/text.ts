import { Expression } from '../expression';

import { TermExpression } from './term';

export class TextExpression extends TermExpression {

  public static fromMatch(match: string) {
    return new TextExpression(match);
  }

  constructor(value: string) {
    super(value.toLowerCase());
  }

  public equals(other: Expression): boolean {
    return this === other || (other instanceof TextExpression && this.value === other.value);
  }

  public toString() {
    return `"${this.value}"`;
  }

}
