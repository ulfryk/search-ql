/* tslint:disable:no-unused-expression no-magic-numbers */
import { expect } from 'chai';

import { DateExpression, NumberExpression, TermExpression, TextExpression } from './index';

describe('SearchQL expressions', () => {

  describe('TermExpression', () => {

    describe('fromMatch() static method', () => {

      const text = TermExpression.fromMatch('aaa');
      const date = TermExpression.fromMatch('2018-07-04');
      const num = TermExpression.fromMatch('-0012.32100');

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

});
