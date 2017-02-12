package test;/*
 * Created by izzielau on 1/26/2017.
 */

import com.qualcomm.robotcore.eventloop.opmode.Autonomous;
import com.qualcomm.robotcore.hardware.DcMotor;
import com.qualcomm.robotcore.hardware.LightSensor;

import opmodes.MochaCalibration;
import team25core.DeadReckon;
import team25core.Drivetrain;
import team25core.FourWheelDirectDrivetrain;
import team25core.GamepadTask;
import team25core.MonitorMotorTask;
import team25core.PersistentTelemetryTask;
import team25core.Robot;
import team25core.RobotEvent;

@Autonomous(name = "TEST Pivot Turns @@@@@", group = "5218")
public class MochaPivotTurnTest extends Robot {

    private enum TurnType {
        LEFT_OVER_LEFT,
        LEFT_OVER_RIGHT,
        RIGHT_OVER_LEFT,
        RIGHT_OVER_RIGHT,
        CUSTOM
    }

    public TurnType turnType;
    public PersistentTelemetryTask persistentTelemetryTask;

    private FourWheelDirectDrivetrain drivetrain;
    private GamepadTask gamepad;

    private DcMotor motorFR;
    private DcMotor motorFL;
    private DcMotor motorBR;
    private DcMotor motorBL;

    private LightSensor rightLight;
    private LightSensor leftLight;

    @Override
    public void handleEvent(RobotEvent e) {
        if (e instanceof GamepadTask.GamepadEvent) {
            GamepadTask.GamepadEvent event = (GamepadTask.GamepadEvent) e;
            handleGamepadSelection(event);
        }
    }

    public void handleGamepadSelection(GamepadTask.GamepadEvent event) {
        switch (event.kind) {
            case BUTTON_X_DOWN:
                turnType = TurnType.RIGHT_OVER_LEFT;
                persistentTelemetryTask.addData("TURN TYPE: ", "RL");
                break;
            case BUTTON_B_DOWN:
                turnType = TurnType.RIGHT_OVER_RIGHT;
                persistentTelemetryTask.addData("TURN TYPE: ", "RR");
                break;
            case BUTTON_Y_DOWN:
                turnType = TurnType.LEFT_OVER_LEFT;
                persistentTelemetryTask.addData("TURN TYPE: ", "LL");
                break;
            case BUTTON_A_DOWN:
                turnType = TurnType.LEFT_OVER_RIGHT;
                persistentTelemetryTask.addData("TURN TYPE: ", "LR");
                break;
            case LEFT_TRIGGER_DOWN:
                turnType = TurnType.CUSTOM;
                persistentTelemetryTask.addData("TURN TYPE: ", "CUSTOM");
                break;
            case RIGHT_TRIGGER_DOWN:
                rightLight.enableLed(true);
                leftLight.enableLed(true);
                break;
            case RIGHT_BUMPER_DOWN:
                rightLight.enableLed(true);
                leftLight.enableLed(true);
                break;
        }
    }

    @Override
    public void init()
    {
        gamepad = new GamepadTask(this, GamepadTask.GamepadNumber.GAMEPAD_1);
        addTask(gamepad);

        persistentTelemetryTask = new PersistentTelemetryTask(this);
        addTask(persistentTelemetryTask);

        persistentTelemetryTask.addData("TURN TYPE: ", "UNSELECTED");

        motorFR = hardwareMap.dcMotor.get("motorFR");
        motorBR = hardwareMap.dcMotor.get("motorBR");
        motorFL = hardwareMap.dcMotor.get("motorFL");
        motorBL = hardwareMap.dcMotor.get("motorBL");

        rightLight = hardwareMap.lightSensor.get("lightRight");
        leftLight = hardwareMap.lightSensor.get("lightLeft");
        rightLight.enableLed(true);
        leftLight.enableLed(true);

        drivetrain = new FourWheelDirectDrivetrain(MochaCalibration.TICKS_PER_INCH, MochaCalibration.PIVOT_MULTIPLIER, motorFR, motorBR, motorFL, motorBL);
        drivetrain.resetEncoders();
        drivetrain.encodersOn();
    }

    @Override
    public void start()
    {
        switch (turnType) {
            case LEFT_OVER_LEFT:
                drivetrain.pivotTurn(Drivetrain.PivotSide.LEFT_OVER_LEFT, 0.3);
                break;
            case LEFT_OVER_RIGHT:
                drivetrain.pivotTurn(Drivetrain.PivotSide.LEFT_OVER_RIGHT, 0.3);
                break;
            case RIGHT_OVER_LEFT:
                drivetrain.pivotTurn(Drivetrain.PivotSide.RIGHT_OVER_LEFT, 0.3);
                break;
            case RIGHT_OVER_RIGHT:
                drivetrain.pivotTurn(Drivetrain.PivotSide.RIGHT_OVER_RIGHT, 0.3);
                break;
            case CUSTOM:
                drivetrain.straight(0.3);
                break;
        }

        addTask(new MonitorMotorTask(this, motorBL));
        addTask(new MonitorMotorTask(this, motorBR));

    }

}
