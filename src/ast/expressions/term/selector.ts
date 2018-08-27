import { TermExpression } from './term';

export class SelectorExpression extends TermExpression {

  public static fromMatch(match: string) {
    return new SelectorExpression(match);
  }

  public static fromTerm({ value }: TermExpression) {
    return SelectorExpression.fromMatch(value);
  }

}
