port module Main exposing (main)

import Browser
import Html exposing (Html, button, div, img, input, label, span, text)
import Html.Attributes exposing (alt, class, disabled, for, id, placeholder, src, type_, value)
import Html.Events exposing (onClick, onInput)
import Process
import Task



-- Port to send data to JavaScript


port sendMessage : String -> Cmd msg



-- Port to receive data from JavaScript


port receiveMessage : (String -> msg) -> Sub msg


type Msg
    = ToggleDropdown
    | SelectOption String String
    | ToggleForm String
    | UpdatePhone String
    | UpdateCardName String
    | UpdateCardNumber String
    | UpdateExpiry String
    | UpdateCVV String
    | StartPayment
    | PaymentConfirmed
    | PaymentFailed
    | ShowPromptSection
    | ShowPleaseWaitSection
    | ShowSuccessSection
    | ShowTimeoutSection


type alias Model =
    { isDropdownOpen : Bool
    , selectedNetwork : String
    , selectedIcon : String
    , activeForm : String
    , phone : String
    , cardName : String
    , cardNumber : String
    , expiry : String
    , cvv : String
    , paymentStatus : PaymentStatus
    , apiKey : String
    , transid : String
    , amount : Float
    , customer_email : String
    , currency : String
    , redirect_url : String
    , pay_button_text : String
    , custom_description : String
    , payment_method : String
    }


type PaymentStatus
    = NotStarted
    | ConfirmingPayment
    | WaitingForPrompt
    | PleaseWait
    | PaymentSuccessful
    | PaymentTimedOut


init : { apiKey : String, transid : String, amount : Float, customer_email : String, currency : String, redirect_url : String, pay_button_text : String, custom_description : String, payment_method : String } -> ( Model, Cmd Msg )
init flags =
    ( { isDropdownOpen = False
      , selectedNetwork = "-- Select a mobile network --"
      , selectedIcon = ""
      , activeForm = if flags.payment_method == "" then "mobile-form" else if flags.payment_method == "card" then "card-form" else "mobile-form"
      , phone = ""
      , cardName = ""
      , cardNumber = ""
      , expiry = ""
      , cvv = ""
      , paymentStatus = NotStarted
      , apiKey = flags.apiKey
      , transid = flags.transid
      , amount = flags.amount
      , customer_email = flags.customer_email
      , currency = flags.currency
      , redirect_url = flags.redirect_url
      , pay_button_text = flags.pay_button_text
      , custom_description = flags.custom_description
      , payment_method = flags.payment_method
      }
    , Cmd.none
    )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        ToggleDropdown ->
            ( { model | isDropdownOpen = not model.isDropdownOpen }, Cmd.none )

        SelectOption text icon ->
            ( { model | selectedNetwork = text, selectedIcon = icon, isDropdownOpen = False }, Cmd.none )

        ToggleForm formId ->
            ( { model | activeForm = formId }, Cmd.none )

        UpdatePhone input ->
            let
                -- Filter out non-numeric characters and limit to 10 digits
                filteredInput =
                    input
                        |> String.filter Char.isDigit
                        |> String.left 10
            in
            ( { model | phone = filteredInput }, Cmd.none )

        UpdateCardName input ->
            let
                -- Filter out unwanted characters (only allow letters, spaces, and hyphens)
                filteredInput =
                    input
                        |> String.filter (\char -> Char.isAlpha char || char == ' ' || char == '-')

                -- Limit to 30 characters
                truncatedInput =
                    String.left 30 filteredInput
            in
            ( { model | cardName = truncatedInput }, Cmd.none )

        UpdateCardNumber input ->
            let
                -- Filter out non-numeric characters
                filteredInput =
                    input
                        |> String.filter Char.isDigit

                -- Limit to 16 digits
                truncatedInput =
                    String.left 16 filteredInput

                -- Format as "0000 0000 0000 0000"
                formattedInput =
                    truncatedInput
                        |> String.toList
                        |> List.indexedMap
                            (\i char ->
                                if i > 0 && modBy 4 i == 0 then
                                    " " ++ String.fromChar char

                                else
                                    String.fromChar char
                            )
                        |> String.concat
            in
            ( { model | cardNumber = formattedInput }, Cmd.none )

        UpdateExpiry input ->
            let
                -- Filter out non-numeric characters
                filteredInput =
                    input
                        |> String.filter Char.isDigit

                -- Limit to 4 digits and insert "/" after 2 characters
                formattedInput =
                    if String.length filteredInput > 2 then
                        String.left 2 filteredInput ++ "/" ++ String.slice 2 4 filteredInput

                    else
                        filteredInput
            in
            ( { model | expiry = String.left 5 formattedInput }, Cmd.none )

        UpdateCVV cvv ->
            let
                -- Filter out non-numeric characters and limit to 3 digits
                filteredCVV =
                    cvv
                        |> String.filter Char.isDigit
                        |> String.left 3
            in
            ( { model | cvv = filteredCVV }, Cmd.none )

        StartPayment ->
            ( { model | paymentStatus = ConfirmingPayment }
            , Cmd.batch
                [ Process.sleep (1 * 60 * 1000) |> Task.perform (\_ -> ShowPromptSection)
                , sendMessage "Starting payment process..."
                ]
            )

        PaymentConfirmed ->
            ( { model | paymentStatus = PleaseWait }
            , Cmd.batch
                [ Process.sleep 5000 |> Task.perform (\_ -> ShowSuccessSection)
                , sendMessage "Payment confirmed!"
                ]
            )

        PaymentFailed ->
            ( { model | paymentStatus = PaymentTimedOut }, Cmd.none )

        ShowPromptSection ->
            ( { model | paymentStatus = WaitingForPrompt }, Cmd.none )

        ShowPleaseWaitSection ->
            ( { model | paymentStatus = PleaseWait }, Cmd.none )

        ShowSuccessSection ->
            ( { model | paymentStatus = PaymentSuccessful }, Cmd.none )

        ShowTimeoutSection ->
            ( { model | paymentStatus = PaymentTimedOut }, Cmd.none )


subscriptions : Model -> Sub Msg
subscriptions _ =
    receiveMessage (\_ -> ToggleDropdown)


view : Model -> Html Msg
view model =
    div [ class "flex items-center justify-center min-h-screen bg-gray-100 p-4" ]
        [ div [ class "bg-white shadow-lg rounded-xl max-w-sm w-full" ]
            [ -- Header and other sections...
              -- Header
              div [ class "flex items-center justify-between border-b border-b-gray-200 mb-4" ]
                [ div [ class "p-4 flex justify-between items-center w-full" ]
                    [ div [ class "flex items-end" ]
                        [ div [ class "border border-gray-200 w-fit rounded-md" ]
                            [ img [ src "./src/assets/plogo.svg", alt "Logo", class "w-12 h-12" ] []
                            ]
                        , div [ class "ml-3" ]
                            [ div [ class "text-lg font-bold" ] [ text "Payswitch Merchant" ]
                            , div [ class "text-white text-[10px] font-[700] p-1 w-fit rounded-md bg-blue-500 flex items-center" ]
                                [ img [ class "w-4 h-4 mr-1", src "./src/assets/check.svg", alt "Check Icon" ] []
                                , text "Verified by PaySwitch"
                                ]
                            ]
                        ]
                    , button [ onClick (ToggleForm "close"), class "text-[#EB1717] text-sm font-[700]" ] [ text "Close" ]
                    ]
                ]
            , -- Payment Sections
              if model.paymentStatus == ConfirmingPayment then
                confirmingPaymentView model

              else if model.paymentStatus == WaitingForPrompt then
                didntGetPromptView

              else if model.paymentStatus == PleaseWait then
                pleaseWaitView

              else if model.paymentStatus == PaymentSuccessful then
                paymentSuccessfulView

              else if model.paymentStatus == PaymentTimedOut then
                transactionTimeoutView

              else
                div []
                    [ div []
                        [ -- Segmented Control
                          div [ class "flex flex-col items-center justify-between" ]
                            [ div [ class "px-4 flex justify-between items-center w-full" ]
                                [ div [ class (if model.payment_method == "both" then "inline-flex h-9 w-full items-baseline justify-start rounded-lg bg-gray-100 p-1" else "hidden") ]
                                    [ button
                                        [ onClick (ToggleForm "mobile-form")
                                        , class
                                            ("group inline-flex items-center justify-center whitespace-nowrap py-2 align-middle font-semibold transition-all duration-300 ease-in-out disabled:cursor-not-allowed min-w-[32px] gap-1.5 text-xs h-7 w-full rounded-md px-3 drop-shadow"
                                                ++ (if model.activeForm == "mobile-form" then
                                                        " bg-white stroke-blue-700 text-slate-950"

                                                    else
                                                        " bg-transparent stroke-slate-400 text-slate-600"
                                                   )
                                            )
                                        ]
                                        [ img
                                            [ src
                                                (if model.activeForm == "mobile-form" then
                                                    "./src/assets/mobile.svg"

                                                 else
                                                    "./src/assets/mobileoutlined.svg"
                                                )
                                            , alt "Mobile Money"
                                            , class "w-4 h-4"
                                            ]
                                            []
                                        , text "Pay with Mobile Money"
                                        ]
                                    , button
                                        [ onClick (ToggleForm "card-form")
                                        , class
                                            ("group inline-flex items-center justify-center whitespace-nowrap py-2 align-middle font-semibold transition-all duration-300 ease-in-out disabled:cursor-not-allowed min-w-[32px] gap-1.5 text-xs h-7 w-full rounded-md px-3 drop-shadow"
                                                ++ (if model.activeForm == "card-form" then
                                                        " bg-white stroke-blue-700 text-slate-950"

                                                    else
                                                        " bg-transparent stroke-slate-400 text-slate-600"
                                                   )
                                            )
                                        ]
                                        [ img
                                            [ src
                                                (if model.activeForm == "card-form" then
                                                    "./src/assets/cardoutlined.svg"

                                                 else
                                                    "./src/assets/card.svg"
                                                )
                                            , alt "Card"
                                            , class "w-4 h-4"
                                            ]
                                            []
                                        , text "Pay with Card"
                                        ]
                                    ]
                                ]
                            ]
                        , div [ class "text-center text-xs px-4 text-[##1E1E1E] font-[600] py-4" ]
                            [ text
                                (if model.custom_description == "" then
                                    "Complete your purchase by providing your payment details."

                                 else
                                    model.custom_description
                                )
                            ]

                        -- Mobile Form
                        , if model.activeForm == "mobile-form" then
                            div [ class "w-full px-4 pt-4" ]
                                [ label [ for "phone", class "block text-xs font-[600] mb-1 text-[#616478]" ] [ text "Mobile Network" ]
                                , div [ class "relative w-full mb-5" ]
                                    [ div
                                        [ onClick ToggleDropdown
                                        , class "w-full p-2 border border-[#EBECF2] rounded-md focus:ring-2 text-sm flex items-center justify-between focus:ring-blue-500 focus:outline-none"
                                        ]
                                        [ text model.selectedNetwork
                                        , img [ src model.selectedIcon, alt "", class "w-4 h-4 hidden" ] []
                                        , img [ src "./src/assets/arrowdown.svg", alt "Dropdown Arrow", class "w-4 h-4" ] []
                                        ]
                                    , if model.isDropdownOpen then
                                        div [ class "absolute top-full left-0 w-full bg-white border border-gray-300 rounded shadow-md z-10" ]
                                            [ div [ onClick (SelectOption "MTN" "./src/assets/momo.svg"), class "flex items-center p-2 text-sm cursor-pointer transition hover:bg-gray-100" ]
                                                [ img [ src "./src/assets/momo.svg", alt "MTN", class "w-5 h-5 mr-2" ] []
                                                , text "MTN"
                                                ]
                                            , div [ onClick (SelectOption "Telecel" "./src/assets/telecel.svg"), class "flex items-center p-2 text-sm cursor-pointer transition hover:bg-gray-100" ]
                                                [ img [ src "./src/assets/telecel.svg", alt "Telecel", class "w-5 h-5 mr-2" ] []
                                                , text "Telecel (Formally Vodafone)"
                                                ]
                                            , div [ onClick (SelectOption "AirtelTigo" "./src/assets/at.svg"), class "flex items-center p-2 text-sm cursor-pointer transition hover:bg-gray-100" ]
                                                [ img [ src "./src/assets/at.svg", alt "AirtelTigo", class "w-5 h-5 mr-2" ] []
                                                , text "AirtelTigo"
                                                ]
                                            , div [ onClick (SelectOption "G-Money" "./src/assets/gmoney.svg"), class "flex items-center p-2 text-sm cursor-pointer transition hover:bg-gray-100" ]
                                                [ img [ src "./src/assets/gmoney.svg", alt "G-Money", class "w-5 h-5 mr-2" ] []
                                                , text "G-Money"
                                                ]
                                            , div [ onClick (SelectOption "Zeepay" "./src/assets/zeepay.svg"), class "flex items-center p-2 text-sm cursor-pointer transition hover:bg-gray-100" ]
                                                [ img [ src "./src/assets/zeepay.svg", alt "Zeepay", class "w-5 h-5 mr-2" ] []
                                                , text "Zeepay"
                                                ]
                                            ]

                                      else
                                        text ""
                                    ]
                                , div [ class "form-section pb-3 gap-2 flex flex-col" ]
                                    [ div []
                                        [ label [ for "phone", class "block text-xs font-[600] mb-1 text-[#616478]" ] [ text "Phone Number" ]
                                        , input
                                            [ type_ "tel"
                                            , id "phone"
                                            , placeholder "E.g 021 123 3456"
                                            , class "w-full p-2 border border-[#EBECF2] rounded-md focus:ring-2 text-sm focus:ring-blue-500 focus:outline-none"
                                            , onInput UpdatePhone
                                            , value model.phone
                                            ]
                                            []
                                        ]
                                    ]
                                ]

                          else
                            text ""

                        -- Card Form
                        , if model.activeForm == "card-form" then
                            div [ class "form-section px-4 pt-4 pb-3 gap-4 flex flex-col" ]
                                [ div []
                                    [ label [ for "card-name", class "block text-xs font-[600] mb-1 text-[#616478]" ] [ text "Card name" ]
                                    , input
                                        [ type_ "text"
                                        , id "card-name"
                                        , placeholder "E.g John Doe"
                                        , class "w-full p-2 border border-[#EBECF2] rounded-md focus:ring-2 text-sm focus:ring-blue-500 focus:outline-none"
                                        , onInput UpdateCardName
                                        , value model.cardName
                                        ]
                                        []
                                    ]
                                , div []
                                    [ label [ for "card-number", class "block text-xs font-[600] mb-1  text-[#616478]" ] [ text "Card Number" ]
                                    , input
                                        [ type_ "tel"
                                        , id "card-number"
                                        , placeholder "0000 0000 0000 0000"
                                        , class "w-full p-2 border border-[#EBECF2] rounded-md focus:ring-2 text-sm focus:ring-blue-500 focus:outline-none"
                                        , onInput UpdateCardNumber
                                        , value model.cardNumber
                                        ]
                                        []
                                    ]
                                , div [ class "flex" ]
                                    [ div [ class "gap-1 flex flex-col" ]
                                        [ label [ for "expiry", class "block text-xs font-[600] text-[#616478]" ] [ text "Expiry" ]
                                        , input
                                            [ type_ "tel"
                                            , id "expiry"
                                            , placeholder "mm/yy"
                                            , class "w-full p-2 border border-[#EBECF2] rounded-md focus:ring-2 text-sm focus:ring-blue-500 focus:outline-none"
                                            , onInput UpdateExpiry
                                            , value model.expiry
                                            ]
                                            []
                                        ]
                                    , div [ class "pl-4 gap-1 flex flex-col" ]
                                        [ label [ for "cvv", class "block text-xs font-[600] text-[#616478]" ] [ text "CVV" ]
                                        , input
                                            [ type_ "tel"
                                            , id "cvv"
                                            , placeholder "000"
                                            , class "w-full p-2 border border-[#EBECF2] rounded-md focus:ring-2 text-sm focus:ring-blue-500 focus:outline-none"
                                            , onInput UpdateCVV
                                            , value model.cvv
                                            ]
                                            []
                                        ]
                                    ]
                                ]

                          else
                            text ""

                        -- Button
                        , div [ class "p-4" ]
                            [ button
                                [ type_ "submit"
                                , class
                                    ("w-full h-11 py-2 text-white font-[600] text-sm rounded-md"
                                        ++ (if
                                                if model.activeForm == "mobile-form" then
                                                    model.selectedNetwork == "-- Select a mobile network --" || model.phone == "" || String.length model.phone < 10

                                                else
                                                    model.cvv == "" || String.length model.cvv < 3 || String.length model.expiry < 5 || String.length model.cardNumber < 16 || model.cardName == "" || model.cardNumber == "" || model.expiry == ""
                                            then
                                                " bg-gray-400"

                                            else
                                                " bg-[#2D61E3] hover:bg-blue-700"
                                           )
                                    )
                                , onClick
                                    (if model.activeForm == "mobile-form" then
                                        StartPayment

                                     else
                                        PaymentConfirmed
                                    )
                                , disabled
                                    (if model.activeForm == "mobile-form" then
                                        model.selectedNetwork == "-- Select a mobile network --" || model.phone == ""

                                     else
                                        model.cvv == "" || model.cardName == "" || model.cardNumber == "" || model.expiry == ""
                                    )
                                ]
                                [ text
                                    (if model.pay_button_text == "" then
                                        "Pay " ++ model.currency ++ " " ++ String.fromFloat model.amount

                                     else
                                        model.pay_button_text ++ " " ++ model.currency ++ " " ++ String.fromFloat model.amount
                                    )
                                ]
                            ]
                        ]
                    ]

            -- Footer
            , div [ class "flex flex-col" ]
                [ div [ class "flex items-center justify-center w-full gap-2 text-xs text-[#1E1E1E] font-[600]" ]
                    (if model.paymentStatus == NotStarted then
                        [ text "Supported Wallets:"
                        , if model.activeForm == "card-form" then
                            div [ class "flex gap-1" ]
                                [ img [ src "./src/assets/mastercard.svg", alt "Wallet 1" ] []
                                , img [ src "./src/assets/visalogo.svg", alt "Wallet 2" ] []
                                ]

                          else
                            div [ class "flex gap-1" ]
                                [ img [ src "./src/assets/momo.svg", alt "Wallet 1" ] []
                                , img [ src "./src/assets/telecel.svg", alt "Wallet 2" ] []
                                , img [ src "./src/assets/at.svg", alt "Wallet 3" ] []
                                , img [ src "./src/assets/zeepay.svg", alt "Wallet 4" ] []
                                , img [ src "./src/assets/gmoney.svg", alt "Wallet 5" ] []
                                ]
                        ]

                     else
                        []
                    )
                , div [ class "text-[11px] text-[#222357] font-[600] flex gap-1 justify-center items-center mb-4 mt-2" ]
                    [ img [ src "./src/assets/lock.svg", alt "Wallet 5" ] []
                    , text "Secured by theTeller"
                    ]
                ]
            ]
        ]


confirmingPaymentView : Model -> Html Msg
confirmingPaymentView model =
    div [ class "flex flex-col pt-5 gap-5 items-center justify-center w-full" ]
        [ img [ src "./src/assets/contactless-payment.gif", alt "Wallet 1", class "w-20 h-20" ] []
        , div [ class "flex flex-col gap-2 items-center justify-center text-md font-[700]" ]
            [ text "Complete on Mobile"
            , div [ class "text-sm font-[600] text-center" ]
                [ div [] [ text "Enter your mobile PIN to complete" ]
                , div [] [ text ("your payment of " ++ model.currency ++ String.fromFloat model.amount) ]
                ]
            ]
        , div [ class "flex flex-col gap-3 items-center justify-center w-full px-4" ]
            [ div [ class "w-full" ]
                [ button
                    [ onClick PaymentConfirmed
                    , class "w-full h-11 py-2 bg-blue-600 text-white font-[600] text-sm rounded-md hover:bg-blue-700"
                    ]
                    [ text
                   
                            "Confirm Payment" 
                        
                    ]
                ]
            , div [ class "w-full" ]
                [ button
                    [ onClick PaymentFailed
                    , class "w-full h-11 py-2 mb-5 bg-white text-gray-700 font-[600] text-sm rounded-md border border-gray-200"
                    ]
                    [ text "Cancel Payment" ]
                ]
            ]
        ]


didntGetPromptView : Html Msg
didntGetPromptView =
    div [ class "flex flex-col pt-5 gap-5 items-center justify-center w-full" ]
        [ img [ src "./src/assets/question.gif", alt "Wallet 1", class "w-20 h-20" ] []
        , div [ class "flex flex-col gap-2 items-center justify-center text-md font-[700]" ]
            [ text "Didn't get a prompt?"
            , div [ class "text-sm font-[600] text-center" ]
                [ div [] [ text "To complete this transaction" ]
                , div [] [ text "1. Dial *110# and" ]
                , div [] [ text "2. Go to option 4 ‘Make payment’" ]
                , div [] [ text "3. Now go to option 8 ‘My Approvals’" ]
                , div [] [ text "Follow the necessary steps then come back" ]
                , div [] [ text "to complete payment." ]
                ]
            ]
        , div [ class "flex flex-col gap-3 items-center justify-center w-full px-4" ]
            [ div [ class "w-full" ]
                [ button
                    [ onClick PaymentConfirmed
                    , class "w-full h-11 py-2 bg-blue-600 text-white font-[600] text-sm rounded-md hover:bg-blue-700"
                    ]
                    [ text "Confirm Payment" ]
                ]
            , div [ class "w-full" ]
                [ button
                    [ onClick PaymentFailed
                    , class "w-full h-11 py-2 mb-5 bg-white text-gray-700 font-[600] text-sm rounded-md border border-gray-200"
                    ]
                    [ text "Cancel Payment" ]
                ]
            ]
        ]


pleaseWaitView : Html Msg
pleaseWaitView =
    div [ class "flex flex-col pt-5 gap-5 items-center justify-center w-full" ]
        [ img [ src "./src/assets/contactless-payment.gif", alt "Wallet 1", class "w-20 h-20" ] []
        , div [ class "flex flex-col gap-2 items-center justify-center text-md font-[700]" ]
            [ text "Please Wait"
            , div [ class "text-sm font-[600] text-center" ]
                [ div [] [ text "We’re confirming your payment." ]
                ]
            ]
        , div [ class "flex flex-col gap-3 items-center justify-center w-full px-4 mb-5" ]
            [ div [ class "w-full" ]
                [ button
                    [ disabled True
                    , class "w-full h-11 py-2 bg-[#616478] text-white font-[600] text-sm rounded-md flex gap-1 items-center justify-center"
                    ]
                    [ img [ src "./src/assets/confirmpayment.gif", alt "Loading", class "h-7" ] []
                    , text "Confirming Payment"
                    ]
                ]
            ]
        ]


paymentSuccessfulView : Html Msg
paymentSuccessfulView =
    div [ class "flex flex-col pt-5 gap-5 items-center justify-center w-full" ]
        [ img [ src "./src/assets/verified.gif", alt "Wallet 1", class "w-20 h-20" ] []
        , div [ class "flex flex-col gap-2 items-center justify-center" ]
            [ div [ class "text-md font-[700] text-center" ] [ text "Payment Made Successfully" ]
            ]
        ]


transactionTimeoutView : Html Msg
transactionTimeoutView =
    div [ class "flex flex-col pt-5 gap-5 items-center justify-center w-full" ]
        [ img [ src "./src/assets/alarm.gif", alt "Wallet 1", class "w-20 h-20" ] []
        , div [ class "flex flex-col gap-2 items-center justify-center" ]
            [ div [ class "text-sm font-[600] text-center" ]
                [ div [] [ text "Transaction timed out, please try another" ]
                , div [] [ text "option" ]
                ]
            ]
        , div [ class "flex flex-col gap-3 items-center justify-center w-full px-4" ]
            [ div [ class "w-full" ]
                [ button
                    [ onClick StartPayment
                    , class "w-full h-11 py-2 bg-blue-600 text-white font-[600] text-sm rounded-md hover:bg-blue-700"
                    ]
                    [ text "Try again" ]
                ]
            , div [ class "w-full text-gray-400 justify-center items-center" ]
                [ 
                     text "Or" 
                ]    
            , div [ class "w-full flex flex-col gap-3" ]
                [ button
                    [ onClick StartPayment

                    -- , onClick (ToggleForm "mobile-form")
                    , class "w-full h-11 py-2 bg-white text-gray-700 font-[600] text-sm rounded-md border border-gray-200"
                    ]
                    [ text "Use a different number" ]
                , button
                    [ onClick StartPayment

                    -- , onClick (ToggleForm "card-form")
                    , class "w-full h-11 py-2 mb-5 bg-white text-gray-700 font-[600] text-sm rounded-md border border-gray-200"
                    ]
                    [ text "Use card instead" ]
                ]
            ]
        ]


main :
    Program
        { apiKey : String
        , transid : String
        , amount : Float
        , customer_email : String
        , currency : String
        , redirect_url : String
        , pay_button_text : String
        , custom_description : String
        , payment_method : String
        }
        Model
        Msg
main =
    Browser.element
        { init = init
        , update = update
        , subscriptions = subscriptions
        , view = view
        }
