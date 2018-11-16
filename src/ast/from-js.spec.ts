/* tslint:disable:no-unused-expression no-array-mutation */
import { expect } from 'chai';

import { ExpressionType, OperatorType, ValueType } from '../common/model';
import { and, andNot, gt, gte, gteR, gtR, like, likeR, lt, lte, lteR, ltR, num, or, phrase, txt } from '../testing/utils';
import { fromJS } from './from-js';

describe('SearchQL ast', () => {

  describe('fromJS', () => {

    const ast = [
      txt('aaa AND ) a:aaaa OR OR OR'),
      and(phrase('aaa'), phrase('bbb')),
      likeR(txt('first_name'), txt('John')),
      like(txt('first_name'), txt('John')),
      ltR(num('123'), num('123')),
      lt(txt('age'), num('50')),
      lteR(num('123'), num('123')),
      lte(txt('age'), num('50')),
      gtR(num('123'), num('123')),
      gt(txt('age'), num('50')),
      gteR(num('123'), num('123')),
      gte(txt('age'), num('50')),
      andNot(phrase('aaa'), phrase('bbb')),
      or(phrase('aaa'), and(phrase('bbb'), phrase('ccc'))),
      or(phrase('aaa'), and(or(phrase('aaa'), and(phrase('bbb'), phrase('ccc'))), phrase('ccc'))),
      andNot(and(phrase('aaa'), phrase('bbb')), phrase('ccc')),
    ];

    ast.forEach(expression => {
      it(`should properly convert JSON created by 'toJS' method back to data (for ${expression})`,
        () => {
          expect(fromJS(expression.toJS()).toJS()).to.deep.equal(expression.toJS());
          expect(fromJS(expression.toJS()).equals(expression)).to.be.true;
        });
    });

    const pojo = {
      operator: {
        token: 'AND',
        type: OperatorType.And,
      },
      returnType: ValueType.Phrase,
      type: ExpressionType.Binary,
      value: [
        {
          operator: {
            token: '&',
            type: OperatorType.And,
          },
          returnType: ValueType.Phrase,
          type: ExpressionType.Binary,
          value: [
            {
              returnType: ValueType.Phrase,
              term: {
                returnType: ValueType.Text,
                type: ExpressionType.Text,
                value: 'aaa',
              },
              type: ExpressionType.Phrase,
              value: 'aaa',
            },
            {
              returnType: ValueType.Phrase,
              term: {
                returnType: ValueType.Text,
                type: ExpressionType.Text,
                value: 'bbb',
              },
              type: ExpressionType.Phrase,
              value: 'bbb',
            },
          ],
        },
        {
          returnType: ValueType.Phrase,
          type: ExpressionType.Not,
          value: {
            returnType: ValueType.Phrase,
            term: {
              returnType: ValueType.Text,
              type: ExpressionType.Text,
              value: 'ccc',
            },
            type: ExpressionType.Phrase,
            value: 'ccc',
          },
        },
      ],
    };

    it('should properly work for manual input', () => {
      expect(pojo).to.deep.equal(ast.slice().pop().toJS());
      expect(fromJS(pojo).equals(ast.slice().pop())).to.be.true;
    });

  });

});
