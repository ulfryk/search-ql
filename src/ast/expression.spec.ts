/* tslint:disable:no-unused-expression */
import { expect } from 'chai';
import { Map } from 'immutable';

import { Expression, ExpressionType, ValueType } from '../common/model';
import { InvalidExpression } from './expressions';
import { fromJS } from './from-js';

const getErrors = (e: Expression) => e.toList()
  .filter(node => node instanceof InvalidExpression)
  .flatMap(({ errors }: InvalidExpression) => errors)
  .toArray();

describe('SearchQL ast: Expression tree', () => {

  xit('should be able to self check types', () => {
    expect(Expression).to.equal(Expression);
  });

  describe('integrity', () => {

    const model = Map<string, ValueType>({
      first_name: ValueType.Text,
    });

    const validDTO = [
      {
        preparedValue: 'ccc',
        returnType: ValueType.Text,
        type: ExpressionType.Text,
        value: 'ccc',
      },
      {
        preparedValue: 123,
        returnType: ValueType.Number,
        type: ExpressionType.Number,
        value: '00123.00',
      },
      {
        preparedValue: 1533195898070,
        returnType: ValueType.Date,
        type: ExpressionType.Date,
        value: '2018-08-02T07:44:58.070Z',
      },
      {
        matchingType: ValueType.Text,
        preparedValue: 'first_name',
        returnType: ValueType.Text,
        type: ExpressionType.Selector,
        value: 'first_name',
      },
      {
        preparedValue: 'lorem ipsum',
        returnType: ValueType.Phrase,
        term: {
          preparedValue: 'lorem ipsum',
          returnType: ValueType.Text,
          type: ExpressionType.Text,
          value: ' lorem ipsum ',
        },
        type: ExpressionType.Phrase,
        value: ' lorem ipsum ',
      },
    ];

    // {
    //   operator: {
    //     token: 'AND',
    //     type: OperatorType.And,
    //   },
    //   returnType: ValueType.Phrase,
    //   type: ExpressionType.Binary,
    //   value: [
    //     {
    //       operator: {
    //         token: '&',
    //         type: OperatorType.And,
    //       },
    //       returnType: ValueType.Phrase,
    //       type: ExpressionType.Binary,
    //       value: [
    //         {
    //           preparedValue: 'aaa',
    //           returnType: ValueType.Phrase,
    //           term: {
    //             preparedValue: 'aaa',
    //             returnType: ValueType.Text,
    //             type: ExpressionType.Text,
    //             value: 'aaa',
    //           },
    //           type: ExpressionType.Phrase,
    //           value: 'aaa',
    //         },
    //         {
    //           preparedValue: 'bbb',
    //           returnType: ValueType.Phrase,
    //           term: {
    //             preparedValue: 'bbb',
    //             returnType: ValueType.Text,
    //             type: ExpressionType.Text,
    //             value: 'bbb',
    //           },
    //           type: ExpressionType.Phrase,
    //           value: 'bbb',
    //         },
    //       ],
    //     },
    //
    //   ],
    // };

    const invalidDTO = [

      // --- TEXT ---
      [{
        preparedValue: ' c c c ',
        returnType: ValueType.Text,
        type: ExpressionType.Text,
        value: 'ccc',
      }, ['Values of TextExpression don\'t match: { value: ccc, preparedValue:  c c c }']],
      [{
        preparedValue: ' c c c ',
        returnType: ValueType.Text,
        type: ExpressionType.Text,
        value: 'ccc',
      }, ['Values of TextExpression don\'t match: { value: ccc, preparedValue:  c c c }']],

      // --- NUMBER ---
      [{
        preparedValue: 123,
        returnType: ValueType.Number,
        type: ExpressionType.Number,
        value: '00123aaa',
      }, [
        'NumberExpression contains a non-number value: "00123aaa".',
        'NumberExpression value ("00123aaa") doesn\'t match preparedValue ("123").',
      ]],
      [{
        preparedValue: 123,
        returnType: ValueType.Number,
        type: ExpressionType.Number,
        value: '00124.00',
      }, ['NumberExpression value ("00124.00") doesn\'t match preparedValue ("123").']],
      [{
        preparedValue: '123',
        returnType: ValueType.Number,
        type: ExpressionType.Number,
        value: '00123.00',
      }, [
        'NumberExpression contains a non-number preparedValue: "123" (string).',
      ]],
      [{
        preparedValue: '01201',
        returnType: ValueType.Number,
        type: ExpressionType.Number,
        value: 'xxe0012300',
      }, [
        'NumberExpression contains a non-number value: "xxe0012300".',
        'NumberExpression contains a non-number preparedValue: "01201" (string).',
        'NumberExpression value ("xxe0012300") doesn\'t match preparedValue ("01201").',
      ]],

      // --- DATE ---
      [{
        preparedValue: 1533195898070,
        returnType: ValueType.Date,
        type: ExpressionType.Date,
        value: '2018-08-01T07:44:58.070Z',
      }, [
        'DateExpression value ("2018-08-01T07:44:58.070Z") doesn\'t match preparedValue ("1533195898070").',
      ]],
      [{
        preparedValue: 1533195898070,
        returnType: ValueType.Date,
        type: ExpressionType.Date,
        value: 'a20180802T07:44:58aaa',
      }, [
        'DateExpression contains a non-date value: "a20180802T07:44:58aaa".',
        'DateExpression value ("a20180802T07:44:58aaa") doesn\'t match preparedValue ("1533195898070").',
      ]],
      [{
        preparedValue: '1533195898070',
        returnType: ValueType.Date,
        type: ExpressionType.Date,
        value: '2018-08-02T07:44:58.070Z',
      }, [
        'DateExpression contains a non-number preparedValue: 1533195898070.',
        'DateExpression value ("2018-08-02T07:44:58.070Z") doesn\'t match preparedValue ("1533195898070").',
      ]],
      [{
        preparedValue: '2018-08-02T07:44:58.070Z',
        returnType: ValueType.Date,
        type: ExpressionType.Date,
        value: 'asd2018-08-02asasd',
      }, [
        'DateExpression contains a non-date value: "asd2018-08-02asasd".',
        'DateExpression contains a non-number preparedValue: 2018-08-02T07:44:58.070Z.',
        'DateExpression value ("asd2018-08-02asasd") doesn\'t match preparedValue ("2018-08-02T07:44:58.070Z").',
      ]],

      // --- SELECTOR ---
      [{
        matchingType: ValueType.Number,
        preparedValue: 'first_name',
        returnType: ValueType.Text,
        type: ExpressionType.Selector,
        value: 'first_name',
      }, ['SelectorExpression is invalid - "matchingType" should equal TEXT but is NUMBER']],
      [{
        matchingType: ValueType.Text,
        preparedValue: 'firstName',
        returnType: ValueType.Text,
        type: ExpressionType.Selector,
        value: 'firstName',
      }, ['SelectorExpression is invalid - "firstName" is not available on model definition']],

      // --- PHRASE ---
      [{
        preparedValue: 'lorem ipsum',
        returnType: ValueType.Phrase,
        term: {
          preparedValue: 'orem psum',
          returnType: ValueType.Text,
          type: ExpressionType.Text,
          value: 'lorem ipsum',
        },
        type: ExpressionType.Phrase,
        value: ' lorem ipsum ',
      }, ['PhraseExpression term error: Values of TextExpression don\'t match: { value: lorem ipsum, preparedValue: orem psum}']],

    ] as [any, string[]][];

    validDTO.forEach(dto => {
      const ast = fromJS(dto);
      const integrityChecked = ast.checkIntegrity(model);
      const integrityTypesChecked = ast.checkIntegrity(model).checkTypes();
      const typesIntegrityChecked = ast.checkTypes().checkIntegrity(model);

      it(`should be able to self check integrity (valid expression: ${ast})`, () => {
        expect(ast).to.be.instanceOf(Expression);
        expect(integrityChecked.isValid(), getErrors(integrityChecked).join(';')).to.be.true;
        expect(integrityTypesChecked.isValid(), getErrors(integrityTypesChecked).join(';')).to.be.true;
        expect(typesIntegrityChecked.isValid(), getErrors(typesIntegrityChecked).join(';')).to.be.true;
      });
    });

    invalidDTO.forEach(([dto, errors]) => {
      const ast = fromJS(dto);
      const integrityChecked = ast.checkIntegrity(model);
      const integrityTypesChecked = ast.checkIntegrity(model).checkTypes();
      const typesIntegrityChecked = ast.checkTypes().checkIntegrity(model);

      it(`should be able to self check integrity (invalid expression: ${ast})`, () => {
        expect(ast).to.be.instanceOf(Expression);
        expect(integrityChecked.isValid()).to.be.false;
        expect(integrityTypesChecked.isValid()).to.be.false;
        expect(typesIntegrityChecked.isValid()).to.be.false;

        expect(getErrors(integrityChecked)).to.deep.equal(getErrors(typesIntegrityChecked));
        expect(getErrors(integrityChecked)).to.deep.equal(getErrors(integrityTypesChecked));
        expect(getErrors(integrityChecked)).to.deep.equal(errors);
      });
    });

  });

});
