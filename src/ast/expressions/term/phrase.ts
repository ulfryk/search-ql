import { Expression, ValueType } from '../../../common/model';
import { fromMatch } from './from-match';
import { TermExpression } from './term';

export class PhraseExpression<T> extends TermExpression<string> {

  public static fromTerm<G, C extends TermExpression<G>>(term: C): PhraseExpression<G> {
    return new PhraseExpression(term);
  }

  public static of<G>(value: string) {
    return new PhraseExpression<G>(fromMatch(value));
  }

  public readonly returnType: ValueType = ValueType.Phrase;

  constructor(public readonly term: TermExpression<T>) {
    super(term.value, term.value.trim());
  }

  public equals(other: Expression): boolean {
    return this === other || (
      other instanceof PhraseExpression &&
      this.preparedValue === other.preparedValue &&
      this.term.equals(other.term)
    );
  }

}
