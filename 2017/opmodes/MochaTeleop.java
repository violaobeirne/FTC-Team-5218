
package opmodes;

/*
 * FTC Team 5218: izzielau, October 30, 2016
 */

import com.qualcomm.ftccommon.Device;
import com.qualcomm.hardware.modernrobotics.ModernRoboticsI2cGyro;
import com.qualcomm.robotcore.eventloop.opmode.Disabled;
import com.qualcomm.robotcore.eventloop.opmode.TeleOp;
import com.qualcomm.robotcore.hardware.DcMotor;
import com.qualcomm.robotcore.hardware.DcMotorController;
import com.qualcomm.robotcore.hardware.DeviceInterfaceModule;
import com.qualcomm.robotcore.hardware.LightSensor;
import com.qualcomm.robotcore.hardware.Servo;

import team25core.DeadmanMotorTask;
import team25core.FourWheelDriveTask;
import team25core.GamepadTask;
import team25core.LimitSwitchTask;
import team25core.PersistentTelemetryTask;
import team25core.Robot;
import team25core.RobotEvent;

@TeleOp(name="5218 Mocha", group = "5218")
public class MochaTeleop extends Robot {

    private final static int LED_CHANNEL = 0;

    private final static double SHOOTER_LOW = MochaCalibration.SHOOTER_LOW_A;
    private final static double SHOOTER_MED = MochaCalibration.SHOOTER_MED_Y;
    private final static double SHOOTER_HIGH = MochaCalibration.SHOOTER_HIGH_X;

    private DcMotor frontLeft;
    private DcMotor frontRight;
    private DcMotor backLeft;
    private DcMotor backRight;
    private DcMotor shooterLeft;
    private DcMotor shooterRight;
    private DcMotor sbod;
    private Servo beacon;
    private DeviceInterfaceModule interfaceModule;
    private LightSensor one;
    private LightSensor two;

    private PersistentTelemetryTask ptt;

    @Override
    public void init()
    {
        // CDIM.
        interfaceModule = hardwareMap.deviceInterfaceModule.get("interface");

        // Light sensors.
        one = hardwareMap.lightSensor.get("lightLeft");
        one.enableLed(false);
        two = hardwareMap.lightSensor.get("lightRight");
        two.enableLed(false);

        // Drivetrain.
        frontRight = hardwareMap.dcMotor.get("motorFR");
        frontLeft = hardwareMap.dcMotor.get("motorFL");
        backRight = hardwareMap.dcMotor.get("motorBR");
        backLeft = hardwareMap.dcMotor.get("motorBL");

        frontRight.setMode(DcMotor.RunMode.RUN_USING_ENCODER);
        frontLeft.setMode(DcMotor.RunMode.RUN_USING_ENCODER);
        backRight.setMode(DcMotor.RunMode.RUN_USING_ENCODER);
        backLeft.setMode(DcMotor.RunMode.RUN_USING_ENCODER);

        shooterLeft = hardwareMap.dcMotor.get("shooterLeft");
        shooterRight = hardwareMap.dcMotor.get("shooterRight");

        shooterLeft.setMode(DcMotor.RunMode.STOP_AND_RESET_ENCODER);
        shooterLeft.setMode(DcMotor.RunMode.RUN_USING_ENCODER);
        shooterRight.setMode(DcMotor.RunMode.STOP_AND_RESET_ENCODER);
        shooterRight.setMode(DcMotor.RunMode.RUN_USING_ENCODER);

        // Hook.
        sbod = hardwareMap.dcMotor.get("brush");

        // Servo.
        beacon = hardwareMap.servo.get("beacon");

        ptt = new PersistentTelemetryTask(this);
        addTask(ptt);
    }

    @Override
    public void handleEvent(RobotEvent e) {

    }

    @Override
    public void start() {
        super.start();

        /* DRIVER ONE */
        // Four motor drive.
        final FourWheelDriveTask drive = new FourWheelDriveTask(this, frontLeft, frontRight, backLeft, backRight);
        this.addTask(drive);

        // SBOD
        DeadmanMotorTask collect = new DeadmanMotorTask(this, sbod, 0.7, GamepadTask.GamepadNumber.GAMEPAD_2, DeadmanMotorTask.DeadmanButton.RIGHT_BUMPER);
        addTask(collect);
        DeadmanMotorTask dispense = new DeadmanMotorTask(this, sbod, -0.7, GamepadTask.GamepadNumber.GAMEPAD_2, DeadmanMotorTask.DeadmanButton.RIGHT_TRIGGER);
        addTask(dispense);

        DeadmanMotorTask collectSlow = new DeadmanMotorTask(this, sbod, 0.35, GamepadTask.GamepadNumber.GAMEPAD_2, DeadmanMotorTask.DeadmanButton.LEFT_BUMPER);
        addTask(collectSlow);
        DeadmanMotorTask dispenseSlow = new DeadmanMotorTask(this, sbod, -0.35, GamepadTask.GamepadNumber.GAMEPAD_2, DeadmanMotorTask.DeadmanButton.LEFT_TRIGGER);
        addTask(dispenseSlow);

        // Shooters
        DeadmanMotorTask shootFastLeft = new DeadmanMotorTask(this, shooterLeft, SHOOTER_HIGH, GamepadTask.GamepadNumber.GAMEPAD_2, DeadmanMotorTask.DeadmanButton.BUTTON_X);
        addTask(shootFastLeft);
        DeadmanMotorTask shootFastRight = new DeadmanMotorTask(this, shooterRight, -SHOOTER_HIGH, GamepadTask.GamepadNumber.GAMEPAD_2, DeadmanMotorTask.DeadmanButton.BUTTON_X);
        addTask(shootFastRight);
        DeadmanMotorTask shootLeft = new DeadmanMotorTask(this, shooterLeft, SHOOTER_MED, GamepadTask.GamepadNumber.GAMEPAD_2, DeadmanMotorTask.DeadmanButton.BUTTON_Y);
        addTask(shootLeft);
        DeadmanMotorTask shootRight = new DeadmanMotorTask(this, shooterRight, -SHOOTER_MED, GamepadTask.GamepadNumber.GAMEPAD_2, DeadmanMotorTask.DeadmanButton.BUTTON_Y);
        addTask(shootRight);
        DeadmanMotorTask shootSlowLeft = new DeadmanMotorTask(this, shooterLeft, SHOOTER_LOW, GamepadTask.GamepadNumber.GAMEPAD_2, DeadmanMotorTask.DeadmanButton.BUTTON_A);
        addTask(shootSlowLeft);
        DeadmanMotorTask shootSlowRight = new DeadmanMotorTask(this, shooterRight, -SHOOTER_LOW, GamepadTask.GamepadNumber.GAMEPAD_2, DeadmanMotorTask.DeadmanButton.BUTTON_A);
        addTask(shootSlowRight);

        this.addTask(new GamepadTask(this, GamepadTask.GamepadNumber.GAMEPAD_1) {
            public void handleEvent(RobotEvent e) {
                GamepadEvent event = (GamepadEvent) e;

                if (event.kind == EventKind.LEFT_TRIGGER_DOWN) {
                    beacon.setPosition(1.0);
                } else if (event.kind == EventKind.LEFT_TRIGGER_UP) {
                    beacon.setPosition(0.5);
                } else if (event.kind == EventKind.LEFT_BUMPER_DOWN) {
                    beacon.setPosition(0);
                } else if (event.kind == EventKind.LEFT_BUMPER_UP) {
                    beacon.setPosition(0.5);
                } else if (event.kind == EventKind.BUTTON_B_DOWN) {
                    drive.slowDown(true);
                    drive.slowDown(0.75);
                } else if (event.kind == EventKind.BUTTON_A_DOWN) {
                    drive.slowDown(true);
                    drive.slowDown(1.0);
                } else if (event.kind == EventKind.RIGHT_BUMPER_DOWN) {
                    one.enableLed(true);
                    two.enableLed(true);

                    telemetry.addData("Light (1): ", one.getRawLightDetected());
                    telemetry.addData("Light (2): ", two.getRawLightDetected());
                }
            }
        });

        addTask(new LimitSwitchTask(this, interfaceModule, 0) {
            @Override
            public void handleEvent(RobotEvent e)
            {
                LimitSwitchEvent event = (LimitSwitchEvent)e;

                if (event.kind == EventKind.CLOSED) {
                    telemetry.addData("Status (1): ", "closed");
                } else if (event.kind == EventKind.OPEN) {
                    telemetry.addData("Status (1): ", "open");
                } else {
                    telemetry.addData("Status (1): ", "unknown");
                }
            }
        });

        addTask(new LimitSwitchTask(this, interfaceModule, 1) {
            @Override
            public void handleEvent(RobotEvent e)
            {
                LimitSwitchEvent event = (LimitSwitchEvent)e;

                if (event.kind == EventKind.CLOSED) {
                    telemetry.addData("Status (2): ", "closed");
                } else if (event.kind == EventKind.OPEN) {
                    telemetry.addData("Status (2): ", "open");
                } else {
                    telemetry.addData("Status (2): ", "unknown");
                }
            }
        });
    }
}
