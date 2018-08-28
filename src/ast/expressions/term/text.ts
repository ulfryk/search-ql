import { TermExpression } from './term';

export class TextExpression extends TermExpression<string> {

  public static fromMatch(match: string) {
    return new TextExpression(match);
  }

  public static fromTerm({ value }: TermExpression) {
    return TextExpression.fromMatch(value);
  }

  constructor(value: string) {
    super(value, value.toLowerCase().trim());
  }

  public toString() {
    return `"${this.value}"`;
  }

}
