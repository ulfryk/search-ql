/* tslint:disable:no-unused-expression no-magic-numbers */
import { expect } from 'chai';
import { Map } from 'immutable';

import { ValueType } from '../../../common/model';
import { ParserConfig } from '../../../config';
import { DateExpression } from './date';
import { fromMatch } from './from-match';
import { NumberExpression } from './number';
import { SelectorExpression } from './selector';
import { TermExpression } from './term';
import { TextExpression } from './text';

const model = Map<string, ValueType>({
  first_name: ValueType.Text,
});

const config = ParserConfig.create({ model });

const getTerm = fromMatch(config);

describe('SearchQL expressions', () => {

  describe('fromMatch() Expression constructor', () => {

    const text = getTerm('aaa');
    const date = getTerm('2018-07-04');
    const num = getTerm('-0012.32100');
    const sel = getTerm('first_name');

    it('should return instance of TextExpression', () => {
      expect(text).to.be.instanceof(TermExpression);
      expect(text).to.be.instanceof(TextExpression);
      expect(date).to.be.instanceof(TermExpression);
      expect(date).to.be.instanceof(DateExpression);
      expect(num).to.be.instanceof(TermExpression);
      expect(num).to.be.instanceof(NumberExpression);
      expect(sel).to.be.instanceof(TermExpression);
      expect(sel).to.be.instanceof(SelectorExpression);
    });

  });

});
