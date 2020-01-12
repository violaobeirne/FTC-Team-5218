package opmodes.LM2;
import com.qualcomm.robotcore.eventloop.opmode.Autonomous;
import com.qualcomm.robotcore.eventloop.opmode.Disabled;
import com.qualcomm.robotcore.hardware.DcMotor;
import com.qualcomm.robotcore.hardware.Servo;
import com.qualcomm.robotcore.util.RobotLog;

import org.firstinspires.ftc.robotcore.external.Telemetry;

import opmodes.calibration.MiyazakiCalibration;
import team25core.DeadReckonPath;
import team25core.DeadReckonTask;
import team25core.GamepadTask;
import team25core.MechanumGearedDrivetrain;
import team25core.Robot;
import team25core.RobotEvent;
import team25core.TankMechanumControlScheme;

/**
 * Created by Lizzie on 11/2/2019.
 */
@Autonomous(name = "5218 LM2 Autonomous")
@Disabled
public class RachmaninoffLM2Autonomous extends Robot {

    // drivetrain and mechanisms declaration
    private DcMotor frontLeft;
    private DcMotor frontRight;
    private DcMotor backLeft;
    private DcMotor backRight;
    private DcMotor leftIntake;
    private DcMotor rightIntake;
    private Servo leftArm;
    private Servo rightArm;
    private Servo stoneArm;
    private MechanumGearedDrivetrain drivetrain;

    // gamepad and telemetry declaration
    private GamepadTask gamepad;
    private Telemetry.Item alliance;
    private Telemetry.Item startPos;
    private Telemetry.Item path;

    // skybridge constant declaration
    private DeadReckonPath moveFoundationPath;
    private RachmaninoffSkybridgePath skybridgePath;
    private RachmaninoffSkybridgePath.AllianceColor allianceColor;
    private RachmaninoffSkybridgePath.StartingPosition startingPosition;

    @Override
    public void init() {
        // drivetrain and mechanisms initialization
        frontLeft = hardwareMap.dcMotor.get("frontLeft");
        frontRight = hardwareMap.dcMotor.get("frontRight");
        backLeft = hardwareMap.dcMotor.get("backLeft");
        backRight = hardwareMap.dcMotor.get("backRight");
        leftIntake = hardwareMap.dcMotor.get("leftIntake");
        rightIntake = hardwareMap.dcMotor.get("rightIntake");
        leftArm = hardwareMap.servo.get("leftArm");
        rightArm = hardwareMap.servo.get("rightArm");
        stoneArm = hardwareMap.servo.get("arm");

        TankMechanumControlScheme scheme = new TankMechanumControlScheme(gamepad1, TankMechanumControlScheme.MotorDirection.NONCANONICAL);
        drivetrain = new MechanumGearedDrivetrain(60, frontRight, backRight, frontLeft, backLeft);
        drivetrain.resetEncoders();
        drivetrain.encodersOn();
        drivetrain.setNoncanonicalMotorDirection();

        // gamepad and telemtry initialization
        gamepad = new GamepadTask(this, GamepadTask.GamepadNumber.GAMEPAD_1);
        addTask(gamepad);

        alliance = telemetry.addData("Alliance: ", "NOT SELECTED");
        startPos = telemetry.addData("Starting Position: ", "NOT SELECTED");
        path = telemetry.addData("Path: ", "NOT SELECTED");

        skybridgePath = new RachmaninoffSkybridgePath();
        moveFoundationPath = new DeadReckonPath();
        allianceColor = allianceColor.DEFAULT;
        startingPosition = startingPosition.DEFAULT;
        leftArm.setPosition(MiyazakiCalibration.ARM_LEFT_STOW);
        rightArm.setPosition(MiyazakiCalibration.ARM_RIGHT_STOW);
        // stoneArm.setPosition(MiyazakiCalibration.ARM_STOW);
    }

    @Override
    public void handleEvent(RobotEvent e) {
        if (e instanceof GamepadTask.GamepadEvent) {
            GamepadTask.GamepadEvent event = (GamepadTask.GamepadEvent) e;
            handleGamePadSelection(event);
        }
    }

    @Override
    public void start() {
        leftArm.setPosition(MiyazakiCalibration.ARM_LEFT_STOW);
        rightArm.setPosition(MiyazakiCalibration.ARM_RIGHT_STOW);
        // stoneArm.setPosition(MiyazakiCalibration.ARM_STOW);
        moveFoundationPath = skybridgePath.getPath(allianceColor, startingPosition);
        moveFoundation(moveFoundationPath);
    }

    public void handleGamePadSelection (GamepadTask.GamepadEvent event) {
        switch (event.kind){
            case BUTTON_X_DOWN:
                allianceColor = allianceColor.BLUE;
                alliance.setValue("BLUE");
                break;
            case BUTTON_B_DOWN:
                allianceColor = allianceColor.RED;
                alliance.setValue("RED");
                break;
            case BUTTON_Y_DOWN:
                startingPosition = startingPosition.BUILDING;
                startPos.setValue("BUILDING SIDE");
                break;
            case BUTTON_A_DOWN:
                startingPosition = startingPosition.LOADING;
                startPos.setValue("LOADING SIDE");
                break;
        }
    }

    public void moveFoundation(final DeadReckonPath path)
    {
        addTask(new DeadReckonTask(this, path, drivetrain) {
            public void handleEvent(RobotEvent e) {
                DeadReckonEvent event = (DeadReckonEvent) e;
                switch(event.kind) {
                    case PATH_DONE:
                        RobotLog.i("163: PATH DONE");
                        // dropFoundationArms(true);
                    case SEGMENT_DONE:
                        RobotLog.i("163: SEGMENT DONE %d", num);
                        if (num == 2) {
                            dropFoundationArms(true);
                        } else if (num == 5) {
                           if (startingPosition == RachmaninoffSkybridgePath.StartingPosition.BUILDING) {
                               dropFoundationArms(false);
                           } else if (startingPosition == RachmaninoffSkybridgePath.StartingPosition.LOADING) {
                               dropStoneArm(false);
                           }
                        }

                }
            }
        });
    }

    public void dropFoundationArms(boolean drop)
    {
        if (drop == false) {
            leftArm.setPosition(MiyazakiCalibration.ARM_LEFT_STOW);
            rightArm.setPosition(MiyazakiCalibration.ARM_RIGHT_STOW);
        } else if (drop == true) {
            leftArm.setPosition(MiyazakiCalibration.ARM_LEFT_DOWN);
            rightArm.setPosition(MiyazakiCalibration.ARM_RIGHT_DOWN);
        }
    }


    public void dropStoneArm (boolean drop) {
        if (drop == true) {
            // stoneArm.setPosition(MiyazakiCalibration.ARM_DOWN);
        }
        if (drop == false) {
            // stoneArm.setPosition(MiyazakiCalibration.ARM_STOW);
        }
    }
}
