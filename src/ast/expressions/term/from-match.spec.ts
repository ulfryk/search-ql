/* tslint:disable:no-unused-expression no-magic-numbers */
import { expect } from 'chai';

import { DateExpression } from './date';
import { fromMatch } from './from-match';
import { NumberExpression } from './number';
import { TermExpression } from './term';
import { TextExpression } from './text';

describe('SearchQL expressions', () => {

  describe('fromMatch() Expression constructor', () => {

    const text = fromMatch('aaa');
    const date = fromMatch('2018-07-04');
    const num = fromMatch('-0012.32100');

    it('should return instance of TextExpression', () => {
      expect(text).to.be.instanceof(TermExpression);
      expect(text).to.be.instanceof(TextExpression);
      expect(date).to.be.instanceof(TermExpression);
      expect(date).to.be.instanceof(DateExpression);
      expect(num).to.be.instanceof(TermExpression);
      expect(num).to.be.instanceof(NumberExpression);
    });

  });

});
