;; 3.4 並列性：時が本質的
;; 状態，同一性および変化の論点の中心は，計算モデルに時間を含めたところである．

;; システムを並列的に動く計算プロセスの集まりとしてモデル化する

;; 3.4.1 並列システムでの時
;; 一般的現象は，複数のプロセスが共通の状態変数を共有することがあるということ．

;; 並列プログラムの正しい振る舞い
;; 並列プログラムに紛れ込む微妙な虫の代表
;; 並列に対する可能な制限の一つは，共有状態変数を変更する２つの演算は，
;; 同時に起きてはいけないと限定する．ただし，これは非常に難しい
;; もう少し厳しくない制限
;; プロセスがある順序で逐次的に走ったのと，同じ結果を生じるように保証すること

;; 問題3.38
;; Peter, Paul, Mary
;; pe,pa,ma -> 45
;; pe,ma,pa -> 35
;; pa,pe,ma -> 45
;; pa,ma,pe -> 50
;; ma,pe,pa -> 40
;; ma,pa,pe -> 40

;; 3.4.2 並列性の制御機構
;; 並列プロセスの混ざり合いを制約する一般的な機構の一つ，直列変換器（serializer）

;; 共有状態へのアクセスの直列化
;; プロセスは並列にじっこうできるが，並列には実行できない手続きの集まりがある．
;; 共有変数へのアクセス制御に直列化を使う．共有変数を，その変数の前の値に基づいて更新しようと思えば，
;; その変数の前の値へのアクセスと，その変数への新しい値の代入とを同一の手続きの中に置く．そうすればその変数に代入する他の手続きは，
;; これらの手続きを同一の直列変換器で直列化することで，この手続きと他の手続きは並列には走れないことが保証できる．

;; Schemeの直列変換器

;; parallel-executeという手続きを取り入れたとする
;; (parallel-execute <p1> <p2> .. <pk>)

;; (define x 10)
;; (define s (make-serializer))
;; (parallel-execute (s (lambda () (set! x (* x x))))
;;                   (s (lambda () (set! x (+ x 1)))))

;; 問題3.39
;; 101 -> set *xx後に+x1
;; 121 -> +x1後にset *xx
;; 100 -> *xx +x1 set

;; 問題3.40
(define x 10)
(parallel-execute (lambda () (set! x (* x x)))
		  (lambda () (set! x (* x x x))))
;; 1000000 -> *xx,*xxx *xxx,*xxでも同じ
;; 100     -> *xxset前に*xxxがset
;; 1000    -> *xxxset前に*xxがset
;; 10000   -> P1がxを２回評価する間にP2がxを1000に変える, P2がxを２回評価後にP1がxを100に変えたケースも一緒
;; 100000  -> P2がxを１回評価後に,P1がxを100に変更

;;1000000

;; 問題3.41
;; 直列化しなくても，もともと一つの手続きなので割り込みようがない
;; 値を参照するだけで，代入とかしないし

;; 問題3.42
;; make-accountを変更し，protectedの呼び出しは，dispatch手続きの外でするようにできる．
;; 口座は，払い出し手続きから呼び出されるたびに，同一の直列化された手続きを返す

;; この変更は安全か．何か違いがあるか．
;; 一緒

;; =====
;; 複数の共有資源を使う複雑さ
;; 共有資源が増えると，困難になる
;; seriarizerを外に出した．
;; 直列化を明示的に管理する役割が移った．

;; 問題3.43
;; 口座A,B,Cで残高が10,20,30
;; 口座の残高交換をしながら複数のプロセスが走る
;; プロセスが逐次的に走ったなら，口座残高は，ある順序で10,20,30である．

;; 最初の版の口座交換プログラムだと，この条件が破られる
;; 残高の合計は保持される
A: 10 ->                           -> 20
B: 20 ->         30      -> 20
C: 30 ->         20      ->
P1:   -> dif(10) ->      -> B with -> A dep
P2:              (BCの交換)
;; 各講座の取引を直列しなければ，この条件も破れる
withdrowの手続きで，subとsetの間でamountの値が変われば，合計値も変わる

;; 問題3.44
;; 移動と交換の間の本質的な違い
交換の場合は２つの口座の残高の差分を持っておく必要があり，交換を直列化しないとこの値が変わりうる

;; 問題3.45
serialized-exchangeを呼び出した時に，
serializerの中でserializerが呼び出されてなんか変なことになりそう
デッドロック

;; 直列変換器の実装
;; 直列変換器はmutexで実装する
(define (make-serializer)
  (let ((mutex (make-mutex)))
    (lambda (p)
      (define (serialized-p . args) ;;.は可変長引数
        (mutex 'acquire)
        (let ((val (apply p args)))
          (mutex 'release)
          val))
      serialized-p)))

(define (make-mutex)
  (let ((cell (list #f)))
    (define (the-mutex m)
      (cond ((eq? m 'acquire)
	     (if (test-and-set! cell)
		 (the-mutex 'aquire)))
	    ((eq? m 'release) (clear! cell))))
    the-mutex))

(define (clear! cell)
  (set-car! cell false))

(define (test-and-set! cell)
  (if (car cell)
      #t
      (begin (set-car! cell true)
	     #f)))
;; test-and-set!はアトミックでないといけない

;; 問題3.46
;; mutexを２つのプロセスが同時に獲得しようとすると，相互排除の実装が崩れてしまう

;; 問題3.47
;; 大きさnのセマフォはmutexの一般化である
;; mutexとtest-and-set!でセマフォを実装せよ

(define (make-semaphore n)
  (let ((counter 0)
        (mutex (make-mutex)))
       (define (the-semaphore m)
         (cond ((eq? m 'acquire)
                (mutex 'acquire)
                (if (> counter n)
                    (begin
                      (mutex 'release)
                      (the-semaphore m)) ; retry
                    (begin
                      (set! counter (+ counter 1))
                      (mutex 'release))))
               ((eq? m 'release)
                (mutex 'acquire)
                (set! counter (- counter 1))
                (mutex 'release))
               (else (error "Unknown message -- SEMAPHORE" m))))
       the-semaphore))

(define (make-semaphore n)
  (let ((counter 0)
        (cell (list #f)))
       (define (the-semaphore m)
         (cond ((eq? m 'acquire)
                (if (or (> counter n) (test-and-set! cell))
                    (the-semaphore m) ; retry
                    (begin
                      (set! counter (+ counter 1))
                      (clear! cell))))
               ((eq? m 'release)
                (set! counter (- counter 1))
                (clear! cell))
               (else (error "Unknown message -- SEMAPHORE" m))))
       the-semaphore))

;; 余談
;; shared lock, shared lock同士なら競合しない．exclusive lockとは競合する
;; mvcc DBでよくやるやり方 readとwriteが競合しないようになる
;; シリアライズする必要があるのか？
;; アイソレーションをどのくらい確保するか，という概念
;; isolation level
;;  read uncommited, read commited, repeatable read, serializable

;; デッドロック
;; 問題3.48
;; 口座に番号付して，若い番号の口座から取得しようとする．
;; デッドロック回避法が交換問題でデッドロックを回避する理由を説明せよ
;; a1,a2のロックは必ずa1からロックを取りに行くようになるため，
;; a2をとってa1待ちの状態が発生しない

(define (exchange account1 account2)
  (let ((difference (- (account1 'balance)
                       (account2 'balance))))
       ((account1 'withdraw) difference)
       ((account2 'deposit) difference)))

(define (make-account-and-serializer balance id)
  (define (withdraw amount)
    (if (>= balance amount)
        (begin (set! balance (- balance amount))
               balance)
        "Insufficient funds"))
  (define (deposit amount)
    (set! balance (+ balance amount))
    balance)
  (let ((balance-serializer (make-serializer)))
       (define (dispatch m)
         (cond ((eq? m 'withdraw) withdraw)
               ((eq? m 'deposit) deposit)
               ((eq? m 'balance) balance)
               ((eq? m 'serializer) balance-serializer)
               ((eq? m 'id) id)
               (else (error "Unknown request -- make-account" m))))
       dispatch))

(define (serialized-exchange account1 account2)
  (let ((serializer1 (account1 'serializer))
        (serializer2 (account2 'serializer)))
       (if (> (account1 'id) (account2 'id))
           ((serializer1 (serializer2 exchange))
            account1
            account2)
           ((serializer2 (serializer1 exchange))
            account2
            account1))))

;; 問題3.49
;; デッドロック回避機構が動かないシナリオ
;; 共有資源Aを取得して，Aの状態によって，もうひとつの共有資源が決まる場合

;; 並列性，時および通信
;; メモリの内容がすべての時点で一貫した状態にあるとは限らない
;; 最近の並列変換器パラダイムは，並列制御の新しい解決法に変わられつつある

;; バリア同期など

;;共有状態の問題点は，巨大分散システムにもある
;;残高が変わったと言えるのは，口座残高が変わった直後，同期されたあと．
;;別の支店からアクセスしたときに，正しい振る舞いになるための制約とはなにか
;;関係するのは，PeterとPaulが個別に見た振る舞いと，同期直後の口座の状態である

;; 並列制御における時の概念は，通信と結びついている
;; 計算モデルで時と状態の扱いで出会った複雑性は，実は物理的宇宙の基本的複雑性を反映している


