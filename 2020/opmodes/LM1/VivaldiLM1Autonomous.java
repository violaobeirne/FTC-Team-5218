package opmodes.LM1;
import com.qualcomm.robotcore.eventloop.opmode.Autonomous;
import com.qualcomm.robotcore.eventloop.opmode.Disabled;
import com.qualcomm.robotcore.hardware.DcMotor;
import com.qualcomm.robotcore.hardware.Servo;
import com.qualcomm.robotcore.util.RobotLog;

import org.firstinspires.ftc.robotcore.external.Telemetry;

import opmodes.calibration.HisaishiCalibration;
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
@Autonomous(name = "5218 LM1 Autonomous")
@Disabled
public class VivaldiLM1Autonomous extends Robot {
    // drivetrain and mechanisms declaration
    private DcMotor frontLeft;
    private DcMotor frontRight;
    private DcMotor backLeft;
    private DcMotor backRight;
    private DcMotor lift;
    private Servo susan;
    private Servo claw;
    private Servo leftArm;
    private Servo rigthArm;
    private MechanumGearedDrivetrain drivetrain;

    // gamepad and telemetry declaration
    private GamepadTask gamepad;
    private Telemetry.Item alliance;
    private Telemetry.Item startPos;
    private Telemetry.Item path;

    // skybridge constant declaration
    private DeadReckonPath initialPath;
    private DeadReckonPath pullBackPath;
    private DeadReckonPath moveUnderBridgePath;
    private VivaldiSkybridgePath skybridgePath;
    private VivaldiSkybridgePath.AllianceColor allianceColor;
    private VivaldiSkybridgePath.StartingPosition startingPosition;

    @Override
    public void init() {
        // drivetrain and mechanisms initialization
        frontLeft = hardwareMap.dcMotor.get("frontLeft");
        frontRight = hardwareMap.dcMotor.get("frontRight");
        backLeft = hardwareMap.dcMotor.get("backLeft");
        backRight = hardwareMap.dcMotor.get("backRight");
        lift = hardwareMap.dcMotor.get("lift");
        susan = hardwareMap.servo.get("susan");
        claw = hardwareMap.servo.get("claw");
        leftArm = hardwareMap.servo.get("leftArm");
        rigthArm = hardwareMap.servo.get("rightArm");

        TankMechanumControlScheme scheme = new TankMechanumControlScheme(gamepad1);
        drivetrain = new MechanumGearedDrivetrain(60, frontRight, backRight, frontLeft, backLeft);
        drivetrain.encodersOn();
        drivetrain.resetEncoders();
        drivetrain.setNoncanonicalMotorDirection();

        // gamepad and telemtry initialization
        gamepad = new GamepadTask(this, GamepadTask.GamepadNumber.GAMEPAD_1);
        addTask(gamepad);

        alliance = telemetry.addData("Alliance: ", "NOT SELECTED");
        startPos = telemetry.addData("Starting Position: ", "NOT SELECTED");
        path = telemetry.addData("Path: ", "NOT SELECTED");

        skybridgePath = new VivaldiSkybridgePath();
        initialPath = new DeadReckonPath();
        allianceColor = allianceColor.DEFAULT;
        startingPosition = startingPosition.DEFAULT;
        pullBackPath = new DeadReckonPath();
        pullBackPath.addSegment(DeadReckonPath.SegmentType.STRAIGHT, 40, 0.4);
        moveUnderBridgePath = new DeadReckonPath();

        leftArm.setPosition(HisaishiCalibration.ARM_LEFT_STOW);
        rigthArm.setPosition(HisaishiCalibration.ARM_RIGHT_STOW);

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
        //arm.setPosition(HisaishiCalibration.ARM_STOW);
        if (allianceColor == allianceColor.RED){
            // moveUnderBridgePath.addSegment(DeadReckonPath.SegmentType.SIDEWAYS, 30, -0.2);
        } else {
            // moveUnderBridgePath.addSegment(DeadReckonPath.SegmentType.SIDEWAYS, 30, 0.2);
        }
        initialPath = skybridgePath.getPath(allianceColor, startingPosition);
        initialMove(initialPath);
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
    public void moveUnderBridge() {
        addTask(new DeadReckonTask(this, moveUnderBridgePath, drivetrain) {
            public void handleEvent(RobotEvent e) {
                DeadReckonTask.DeadReckonEvent event = (DeadReckonTask.DeadReckonEvent) e;
                switch(event.kind) {
                    case PATH_DONE:
                        RobotLog.i("163: PATH DONE");
                }
            }
        });
    }
    public void pullBack() {
        addTask(new DeadReckonTask(this, pullBackPath, drivetrain) {
            public void handleEvent(RobotEvent e) {
                DeadReckonTask.DeadReckonEvent event = (DeadReckonTask.DeadReckonEvent) e;
                switch(event.kind) {
                    case PATH_DONE:
                        RobotLog.i("163: PATH DONE");
                        leftArm.setPosition(HisaishiCalibration.ARM_LEFT_STOW);
                        rigthArm.setPosition(HisaishiCalibration.ARM_RIGHT_STOW);
                        moveUnderBridge();
                }
            }
        });
    }
    public void initialMove(final DeadReckonPath path) {
        addTask(new DeadReckonTask(this, path, drivetrain) {
            public void handleEvent(RobotEvent e) {
                DeadReckonTask.DeadReckonEvent event = (DeadReckonTask.DeadReckonEvent) e;
                switch(event.kind) {
                    case PATH_DONE:
                        RobotLog.i("163: PATH DONE");
                        leftArm.setPosition(HisaishiCalibration.ARM_LEFT_DOWN);
                        rigthArm.setPosition(HisaishiCalibration.ARM_RIGHT_DOWN);
                        pullBack();
                }
            }
        });
    }
}
